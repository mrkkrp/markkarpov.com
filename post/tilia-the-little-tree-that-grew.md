---
title: Tilia: the little tree that grew
desc: A follow up post about the new formatter for Haskell, addressing popular questions and giving an update about its development.
date:
  published: November 15, 2026
tag: haskell
---

A couple months ago I [announced][tilia-announcement] a new formatter for
Haskell that I wrote from scratch—[Tilia][tilia-repo]. Since then I received
a number of emails asking for more information about the project, an issue
was opened on Tilia's tracker requesting comparisons with Ormolu/Fourmolu,
and I think a similar question was a popular comment on Reddit. Two days
after the announcement I was contacted by brave maintainers of Clash
compiler who decided to format their code base with Tilia. They contributed
some fixes and requested a change in formatting, which I accepted. This was
followed by two busy months during which I was actively improving virtually
every aspect of Tilia from performance to CPP support to comment handling.
In this post I will share the most important updates and answer some popular
questions with examples.

## Case study: Clash compiler

Soon after the announcement blog post was published the maintainers of Clash
(which is a Haskell to VHDL/Verilog/SystemVerilog compiler) opened a PR with
various fixes. They indicated that they had a branch attempting to use
Fourmolu (apparently open since 2024), but their CPP was more than that tool
could handle, even with a generous amount of `{- FOURMOLU_DISABLE -}`. They
tried the initial release of Tilia and took the trouble of fixing a few
problems that got in the way. After those bugs were solved they found that
the project formatted correctly despite its generous use of CPP.

The one stylistic change that was requested is this.

```haskell
```

## The status of CPP support

CPP was my primary focus after the initial release. The implementation went
through several redesigns since then and now I can declare that it is fairly
robust. I know this because I can format 99% of Hackage modules with Tilia
now. The remaining 1% being genuine declines where for example a component
is marked as non-buildable so its dependencies won't be in the build plan
and so their fixities won't be available, or particularly nasty instances of
CPP function-like macros used in most inconvenient (for a formatter!)
places.

Tilia can now format such non-trivial projects as `QuickCheck`, `yesod`, and
`lens` just the way they are in their repositories, without any adjustments
whatsoever. This was not possible for any Haskell formatter before.

Interaction of comments with CPP and “duplication” were two particularly
nasty problems. Duplication happened when the merging algorithm could not
quite convince itself that two CPP configurations are similar enough, so it
would keep them separate:

TODO examples

Which is not what you want.

Both problems have been resolved now. All instances of CPP use with or
without comments are idempotent, look good, and are AST-preserving across
all of their configurations.

## The status of comment handling

Comment handling in Tilia has reached absolute fidelity—no comments move in
unexpected ways or disappear. This is again based on the results from
extensive Hackage scans which cover several thousands packages. I worked on
the problem of comment placement in [August][fixed-everything]. That work
resulted in Ormolu 0.9.0.0 and Fourmolu picked up the improvements in
0.21.0.0. As nice as that change was, it did not fix *quite everything*, so
I kept iterating on this, now in Tilia, until I got it perfect.

## Why knowing operator fixities is important

One question I received through emails is why knowing operator fixities is
so important. Let me answer with an example. Servant is formatted with
Fourmolu and it is notorious for having many operators, most interestingly
on the type-level, so we can take an example from it:

```haskell
```

Tilia rewrites this as:

```haskell
```

Which is the expected output here. This kind of groupping is why knowing
fixities is necessary. We can ask Tilia for more information with
`--debug-fixity`:

As you can see it knows quite a bit about all operators involved. While
Ormolu/Fourmolu use a built-in dictionary of fixities obtained from
scrapping Hoogle at development time, Tilia infers fixities the way GHC does
directly from source or interface files of dependencies. This is explained
in more details in the [announcement blog post][tilia-announcement]. In
practical terms, it means that fixities are always correct no matter where
they come from (dependencies or your own project) without the need to
maintain the `fixities` field in `fourmolu.yaml` by hand.

## HLS plugin

Tilia now has an HLS plugin. The most interesting feature of this plugin is
that it sources fixity info directly from the GHC session HLS already has.
In this mode of operation it is just as precise, but is also significantly
faster because it does not need to work out fixities on its own. While
Ormolu/Fourmolu could (with some work) also source fixities from HLS
(although they would need to introduce support for fixity namespaces first),
the technical challenge for those tools is in achieving matching precision
outside of HLS, which is what the previous section was about.

## First-class Stack and hpack support

Stack and hpack users can use Tilia seamlessly now.

## Opt-in support for the leading comma style

Now with all the comment/CPP/fixity machinery working quite well I want to
concentrate on aligning Tilia with how people actually write Haskell. Based
on a statistical analysis of Haskell code and `fourmolu.yaml` configurations
found in the wild, I identified leading commas as the most common difference
between what people write and what Tilia outputs. On this ground I changed
“admits no configuration” stance into “admits limited configuration” and
added some configuration options in Tilia 0.2.0.0. Leading commas is one
such option. So, if you want to have leading commas you can with the
`leading-commas` setting:

## Automatic breaking of excessively long lines

It can be annoying to “nudge” your formatter to break excessively long
lines, so this now happens by default. I know Fourmolu had this feature
since XXXX version, and it warns about possible idempotence problems when it
is enabled. The feature is always on in Tilia and I have not identified any
problems with idempotence so far. It is controlled by the `column-limit`
setting:

## Configurable indentation step

Indentation step is configurable now. The default is 2 and you can change it
with the `indentation-step` option:

## Groupping-aware import sorting

Imports in different import groups separated by blank lines will sort
individually now and won't be merged together. This way projects that prefer
to have separate import groups for third-party and first-party modules are
no longer inconvenienced.

## More intelligent preservation of declaration groupping

Based on statistical analysis of Haskell code found in the wild, single-line
top-level declarations without Haddocks are often groupped together:

```haskell
```

These groupping are now preserved in the output.

## Conclusion

Tilia is now much more robust and is extensively tested. Comments, fixities,
and CPP are all solved. With these problems out of the way, the direction I
am taking now is towards eliminating friction between what Tilia outputs and
what people tend to write. This requires a balance: adding a single Boolean
configuration switch doubles the space Tilia needs to test. Leading commas
seem to be worth it, time will tell what else is. One thing I need to be
clear about: correctness is important for me, so there won't be many
configuration options, but I'd like to have something that everyone in the
community could enjoy using.

Finally, I want to say that I am very happy with how this project turned
out. Working on a Haskell formatter takes a lot of effort and meticulous
attention to details. It is impossible to do without being passionate about
it. This work is mostly done in the evenings and in the weekends, and
sometimes the sole recompense is seeing it working well and being able to
improve it in the next version. This is what I will keep doing, with Tilia.

[tilia-announcement]: /post/announcing-tilia.html
[tilia-repo]: https://github.com/mrkkrp/tilia
[fixed-everything]: /post/ormolu-one-refactor-that-fixed-everything.html
