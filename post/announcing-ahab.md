---
title: Announcing Ahab
desc: Announcing Ahab—a hermeticity analyzer for Bazel.
date:
  published: August 14, 2026
  updated: August 21, 2026
tag: bazel
---

> Hark ye yet again,—the little lower layer. All visible objects, man, are
> but as pasteboard masks. But in each event—in the living act, the
> undoubted deed—there, some unknown but still reasoning thing puts forth
> the mouldings of its features from behind the unreasoning mask. If man
> will strike, strike through the mask!
>
> *Herman Melville, Moby-Dick, 1851*

Bazel's entire value proposition—correct caching, remote execution,
trustworthy incremental builds—rests on hermeticity, yet Bazel gives you
almost nothing with which to check it in a systematic, principled fashion. I
have seen the same request many times while doing Bazel consulting: a
company has migrated to Bazel on their own, but the benefits are not
visible. Cache hit rates are low, management starts to question the
investment, and that is when the company decides to reach out to Bazel
experts to understand what went wrong.

What do the experts do? One robust way to identify the problem is to record
the execution log several times and diff it—provided that you have chosen
your execution environment wisely and you know what kind of differences you
are looking for. In practice an execution log, even in the binary protobuf
format, easily reaches 100 GB and more for a non-trivial build. It is
time-consuming to produce and the tooling struggles with that much data. So
what happens next is that you pick a few well-chosen targets of interest and
zoom in on them. Fix a problem, move to the next one, until the situation
seems good enough. Maybe you study the cache hit rate statistics from your
RBE solution to guide the search.

That is useful work, but can we do better? What we would really like is a
guarantee that we have covered *all* instances of hermeticity and
reproducibility problems—ideally without rebuilding an expensive monorepo
from scratch many times over.

## A static analyzer for hermeticity problems

Ahab (Advanced Hermeticity Analyzer for Bazel) is motivated by this set of
constraints:

* **Totality**: report everything and let users ignore what they are fine
  with, via an explicit exception policy. Explicit is better than implicit.
* **Speed**: the check should be fast enough to run in CI on every commit,
  if the user wants it.

The constraints dictate the design: Ahab has to be a kind of static analyzer
working on Bazel's build actions. Bazel already exposes this information via
`bazel aquery`. So, the tool would be about consuming those descriptions and
then reporting anything that seems troublesome. Simple? Well, the devil is
in the details.

Here is what it looks like when it has something to say:

```
Error: found 4 distinct hermeticity violations (6 occurrences):
  1. hermeticity violation: USER leaked into an argument of CppCompile
     action for target //src:lib: -DBUILD_USER=ahab-sentinel-user-4f8a1c6b
  2. ×3 hermeticity violation: Genrule action for target //tools:gen
     declares "requires-network", so the build itself says it cannot run
     like an ordinary action
  3. hermeticity violation: Genrule action for target //:artifact runs
     program "/bin/bash", which comes from outside the build
  4. reproducibility violation: TsProject action for target //ts:app runs
     program "@+typescript//tsc_/tsc" non-reproducibly: it was given an
     option that breaks it, --generateTrace

  But what's this long face about, Mr. Starbuck; wilt thou not chase bash?
```

Yes, the captain has something to say about your build too.

The first time I ran Ahab on itself it flagged a genuine hermeticity
problem. I had forgotten to register a hermetic C++ toolchain, so Rust
linking was reaching for whatever compiler the machine happened to have, and
it said so right away with the correct explanation. At that moment I knew I
had to see how far I could take this.

In the next sections I will talk more about how I built Ahab and why I did
what I did. I will conclude with the roadmap as I see it right now. The
[GitHub repository][ahab-repo] has a comprehensive readme describing
everything you need in order to use the tool, and I do not want to duplicate
that information here.

## The birth of reproducibility specs

Clearly Ahab should hunt for things like leaked absolute paths, `USER` and
`HOSTNAME`, and a surprising number of other things—execution requirements
that turn off the sandbox and actions that read Bazel's workspace status
files. All of that is potentially of interest. But at the end of the day the
elephant in the room is the programs we actually run. The reproducibility of
an action depends on what you run in that action.

At first this is a discouraging realization: we need to know the
reproducibility conditions of every tool that any Bazel build could invoke.
Just how many could that be—hundreds? It is clear that the library of
reproducibility knowledge is what decides the credibility of a tool like
this, so it had better be accurate.

Well, with AI and a competent driver this turns out to be a feasible
project. And the number of tools across all the mainstream rule sets is
actually quite manageable.

We can imagine the library as a map from `ProgramId` to
`ReproducibilitySpec`. The first problem is deciding what a `ProgramId` is.
The first intuition is that it should be the name of the program we run,
like `gcc` or `protoc`. This is not good enough: there can be, and there
are, completely different tools with identical names. The next idea is to
identify tools by their Bazel labels. Again not good enough, because labels
are not stable—they carry such things as the module version and the name the
*consuming* project happened to pick. In the end this definition works well:

```rust
pub enum Origin {
    /// The main repository—the workspace being analyzed. `extension` is set
    /// when the program lives in a repository generated by an extension
    /// that the main repository itself defines.
    Main {
        /// The module extension that generated the repository, if any.
        extension: Option<String>,
    },
    /// An external Bazel module, named as it is in the registry
    /// (`rules_rust`, `llvm`)—never by the apparent name a `bazel_dep` may
    /// have bound it to.
    Module {
        /// The module name, i.e. its `module(name = …)`.
        name: String,
        /// The module extension that generated the repository, if any.
        extension: Option<String>,
    },
    /// Outside the execution root: an absolute path to a tool on the host,
    /// or a bare command name resolved through `PATH`. Either way the
    /// program is not part of the build, which is itself a hermeticity
    /// signal.
    System,
}

pub struct ProgramId {
    /// The repository the program comes from.
    pub origin: Origin,
    /// The program's path within that repository, e.g.
    /// `util/process_wrapper/process_wrapper`. For [`Origin::System`] this
    /// is the path or command name as it appeared in `argv[0]`.
    pub path: String,
}
```

What survives normalization is exactly the part the *tool's own author*
controls: the module name, the extension name, and the path within the
repository.

What about `ReproducibilitySpec`? I started out naively, with something like
this:

```rust
pub enum Reproducibility {
    /// The program is always reproducible, regardless of how it is invoked.
    Always,
    /// The program is never reproducible; no set of flags can make it so.
    Never,
    /// The program is reproducible only under some conditions—see the
    /// requirements and prohibitions of the [`ReproducibilitySpec`].
    Sometimes,
}

pub type Recognize = Arc<dyn Fn(&str) -> Option<String> + Send + Sync>;

pub struct ReproducibilitySpec {
    /// The baseline reproducibility of the program.
    pub reproducibility: Reproducibility,
    /// Flags that are required for the program to be reproducible.
    pub required_flags: BTreeSet<String>,
    /// Flags that break the program's reproducibility.
    pub breaking_flags: BTreeSet<String>,
    /// Map a raw argument to the canonical option it represents, or `None`
    /// if it is not recognized as an option of this program.
    pub recognize: Recognize,
}
```

It turned out to be too crude a model, but it was a start.

## The fishery

Ahab is the kind of tool where you have to get the foundation right and then
it is a matter of throwing it at a lot of real-world projects and addressing
the findings patiently and systematically. To this end I made [the
fishery][the-fishery]. The fishery is a harness that fetches a real-world
Bazel project or rule set at a pinned commit, runs Ahab against it, and
records the report. That recording is the point: a change to a check can
then be judged by what it does to somebody else's build, and a check that
quietly starts flagging four hundred more things says so in the diff rather
than in production. It is both a powerful integration test and a way to
discover what is actually out there.

I currently run Ahab against thirteen projects and rule sets on every
commit—[Abseil][abseil], [Dagger][dagger], [Envoy][envoy],
[NativeLink][nativelink] and [buildtools][buildtools], plus rule sets for
Go, JavaScript, TypeScript, foreign C/C++, packaging, and containers—and I
have only just started. I have to compliment the NativeLink project here:
Ahab could not find much to complain about, apart from four uses of
`/bin/bash` which, one would argue, are fairly benign.

Through the fishery I discovered that I needed first-class support for
synonyms and wrappers in the library—the same tool arrives under several
identities, and half the interesting programs are launchers that hand off to
something else. I discovered that absolute paths are perfectly legitimate in
arguments when you are building containers with `rules_oci` or `rules_img`.
And I discovered that a fairly sophisticated little language is needed to
describe compilers that operate in several modes, since they become
non-reproducible depending on the mode you invoke them in, and each mode
requires its own "neutralizing" flags for the invocation to remain
reproducible. Those are the kinds of things I did not think about when I
started, and they can easily make a tool like this a pain to use if they
remain unaddressed. This is the bulk of the work, and it will continue.

I have to say it—Ahab would not have been possible without AI. It is simply
too ambitious a project to write manually. I estimate that it would have
taken at least a year of unglamorous manual labor to get where I got in a
couple of weeks. It still took a great deal of thinking and hard human work
to direct it all, regardless.

I said earlier that the reproducibility library is what decides the
credibility of a tool like this, so it is fair to ask whether we can trust
AI-generated reproducibility specs. The answer is that no entry is added
unverified. A specification is either read out of the tool's own source or
it is measured, by running the tool twice under conditions that differ in
exactly one respect and comparing the bytes. That is how I know that
`bsdtar`'s gzip output changes when nothing else does, and that `rustdoc`'s
does not. The entry then has to survive the fishery, where it is applied to
somebody else's build and the diff is there for anyone to read. AI made the
volume of that work possible; it is not what makes any particular claim
true.

## Extensibility

Ahab is fully extensible. It caters to the kind of user who is serious about
getting hermeticity right and is willing to invest a little time to get
there. This is why Ahab can load reproducibility specs from JSON, with
nearly all of the expressivity available in the Rust source.

The simplest kind of spec says that a program is fine as long as it is told
what to do about time:

```json
{
  "programs": {
    "@acme+tools//bin/mkarchive": {
      "spec": {
        "reproducibility": "sometimes",
        "required_flags": ["--mtime=*"],
        "breaking_flags": ["--preserve-mtime"]
      }
    }
  }
}
```

Here we use patterns rather than flag names, because what makes an
invocation reproducible is usually a flag *and* its value.
`--remap-path-prefix` says only that some remapping happens;
`--remap-path-prefix=${pwd}=*` says that the execution root is what gets
remapped, which is the thing actually worth requiring.

Ahab also supports exceptions, which can be as broad or as fine-grained as
you like. Here is a real one, from the fishery entry for `buildtools`:

```json
{
  "exceptions": [
    {
      "reason": "the Go tooling links with an autoconfigured toolchain",
      "kind": "bad_path",
      "mnemonic": "Go*"
    }
  ]
}
```

Both formats go further than these two examples: clauses can be guarded the
way the compiler case above needs, a program can be declared to behave like
another or to hand off to whatever it wraps, and an exception can be
narrowed by mnemonic, target, program, or location. The readme documents
every field.

## Conclusion and next steps

Ahab is now available in the Bazel Central Registry. Instructions for a
quick start are on [GitHub][ahab-repo], in the project's readme. It is just
the beginning, of course, so it would be unrealistic to expect the
ergonomics of a seasoned tool right from the start. However, it has
everything it needs to get there in time.

Growing the fishery is the primary objective. So far it leans on rule sets,
which are convenient because they exercise their own tools deliberately, but
a rule set is not what most people build. What I want next is more real
projects, which is the only way to calibrate what a typical build actually
looks like. Beyond that, there is one expressiveness gap I know about and
have not closed: rule sets that generate a launcher script into *your*
package, under *your* target's name, produce a program that no stable
identifier can name, so Ahab reports it as unknown even when the script
really comes from the rule set. And there is the ordinary long tail of tools
nobody has described yet, which is what the fishery keeps finding.

I am happy to accept issues and PRs from the first real users. I believe the
approach behind Ahab is solid and that it scales. I'll run it round the next
monorepo, and round the one after that, and round perdition's flames before
I give it up.

[ahab-repo]: https://github.com/mrkkrp/ahab
[the-fishery]: https://github.com/mrkkrp/ahab/tree/master/fishery
[abseil]: https://github.com/abseil/abseil-cpp
[buildtools]: https://github.com/bazelbuild/buildtools
[dagger]: https://github.com/google/dagger
[envoy]: https://github.com/envoyproxy/envoy
[nativelink]: https://github.com/TraceMachina/nativelink
