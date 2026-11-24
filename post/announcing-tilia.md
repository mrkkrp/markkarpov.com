---
title: Tilia—a new formatter for Haskell
desc: Announcing Tilia—a new formatter for Haskell source code.
date:
  published: September 16, 2026
tag: haskell
---

Haskell remains a difficult programming language to format. The core
parsing/printing machinery can be built relatively easily now that we have
`ghc-lib-parser`, which exposes GHC's real parser (printing was never a
problem), but for years there were three challenges that seemed
insurmountable:

1. Correct handling of comments. I worked on that last month and I am
   convinced that I have learned enough to declare this challenge solved.
2. Formatting operator chains or, more precisely, the uncomfortable question
   of inferring operator fixities with absolute precision.
3. CPP support.

Tilia is a new formatter I wrote from scratch. It aims to solve all three
challenges in a principled way. In this post I am going to present Tilia by
looking at problems 2 (operator fixities) and 3 (CPP support) specifically.
Let's dive in!

## Fixities

How wonderful it would be for the authors of Haskell formatters if there
were no operators with custom fixities! It seems like a small feature of the
language, but it is a very uncomfortable one when it comes to formatting. In
the general case, given an operator chain, one needs to know the precedence
of the operators in order to format it. How do you discover that
information? Well, there are two approaches I know of:

1. You hardcode them. This can be more or less fancy. In the fanciest
   scenario you scan all of Hackage the best you can and assemble a little
   database, which you then ship with your formatter. Of course, that does
   not help with the custom operators your users may have, so you also allow
   them to inform the formatter about fixities by hand. Re-exports are
   another annoying detail, so you allow specifying those too.
2. You make friends with GHC and Cabal and get the real information,
   handling all the intricacies of re-exports and everything else, while
   also harvesting the fixities of operators from the source code you are
   formatting.

To my knowledge, approach 2 has never been attempted. Having implemented it
now end-to-end in Tilia, I think I see why. It is a rough ride!

![How Tilia discovers fixities](/static/img/tilia-fixities.svg)

For a Haskell dependency there are two viable ways to discover its fixities:

* If it is already installed—a boot library shipped with the compiler, or
  anything else that is present, as in a Nix shell—then you can read the
  interface files.
* If it is not installed, then you first need to get its source tarball and
  dig from there.

In the general case, for this to work, both routes must be very well
supported. The interface route is by far the easier one: it returns
information you can consume right away—since GHC has already compiled the
package, you do not need to worry about CPP, or about its source files being
`.hsc`, or about many other fiddly details. When you consume sources
directly you need to worry about all of that.

One thing that helps is that fixities are a syntactic feature of the module
where they are defined, so nothing needs to be compiled—that would be quite
an annoying requirement just to run a formatter! All you need to do is
follow the re-export chains (or rather graphs) until you have discovered all
the operators (while handling cycles all around). Then you cache the result
(another fun aspect, which in the interest of space I will skip). But wait,
there is more. Consider this:

```haskell
import Data.List.NonEmpty (NonEmpty (..))

myFunc x y = x :| y
```

We all know that `:|` is a data constructor of `NonEmpty`, but that is not
obvious from the import alone: the import list never mentions `:|`, only the
type whose `(..)` brings it in. So it is not enough to know which names a
module exports. You also need to track:

* **Re-exports**, including the cycles they form.
* **Children**: what each exported type or class carries with it—data
  constructors, record fields, class methods, associated types—so that
  `T(..)` can be expanded into the names it actually brings into scope.
* **Namespaces**: Haskell has two, and the same operator may be declared in
  both. `:|` in a type and `:|` in an expression need not have the same
  fixity, so a fixity is only ever answered for a particular namespace.
* **How the import was written**: qualified or not, under which alias, with
  an explicit list or a `hiding` clause. All of that decides whether a given
  import could have brought the operator in at all.

Easy.

Finally, you absolutely cannot ignore anything, because what if that one
weird module you skipped defines the operator you are looking for—or a
different operator spelled the same way? In the latter case you have an
ambiguity, and you have to report it rather than guess. These are the rules
if you want to stick with the absolute correctness/precision™ promise.

Given all of the above, it should not come as a surprise that Tilia ended up
living in a kind of symbiotic (or parasitic?) relationship with Cabal. It
will check its build plan and ask it to solve a new one if needed. It will
also ask it to download source tarballs. All of that is a one-time cost and
takes a few seconds at most, even for large projects you have not built
before.

The CLI of Tilia is built in terms of Cabal components:

```bash
$ tilia inplace [COMPONENT] # format all files of COMPONENT in place
$ tilia check   [COMPONENT] # check that all files of COMPONENT are formatted
```

`COMPONENT` may be omitted, in which case it defaults to `all`. For example,
in the case of Tilia itself the valid choices are:

* `all`
* `tilia`, the package, which means every component of it
* `lib:tilia` or `tilia:lib:tilia`
* `exe:tilia` or `tilia:exe:tilia`
* `test:tests` or `tilia:test:tests`, or just `tests`

The Nix use case works flawlessly, because there we read interface
files—there is nothing to download. Stack projects tend to just work, even
though there is currently no Stack-specific support. Finally, private
dependencies provisioned via GitHub or arbitrary URLs in `cabal.project`
also work.

## CPP

CPP is a first-class formattable construct in Tilia. This was a feature I
really wanted to implement, and the one for which I had to go back to the
whiteboard. I ended up reading papers on SuperC ([*Parsing All of C by
Taming the Preprocessor*][superc], Gazzillo & Grimm, PLDI 2012) and TypeChef
([*Variability-Aware Parsing in the Presence of Lexical Macros and
Conditional Compilation*][typechef], Kästner et al., OOPSLA 2011), as well
as the choice calculus of Erwig and Walkingshaw ([*The Choice Calculus: A
Representation for Software Variation*][choice-calculus], TOSEM 2011).

The difficulty specific to Haskell is that there is no parser that supports
CPP. What I mean is that you do not get AST nodes saying “here is a CPP
conditional” and such. Modifying GHC's parser is out of the question. So
what to do?

The approach I settled on is the following:

1. Enumerate the CPP configurations of the module we are formatting.
2. Derive a version of the module per configuration by blanking the lines
   that are not present in it. Mark every configuration with a decision
   vector recording which branches it took.
3. Feed that into the normal parser. There is no CPP left in it, so it
   works.
4. Render each result to an intermediate datatype—something Tilia has been
   doing from the beginning anyway.
5. Traverse all the rendered results and *intelligently* (a word so easy to
   write in a blog post, and God only knows what a pain it is to implement)
   join them back together, reintroducing CPP directives where they are
   needed, purely on the basis of the actual differences between the
   formatted configurations.

And, you may be surprised, but this works, and it is practical. Let's take a
look at an example:

```haskell
{-# LANGUAGE CPP #-}

module Database.Pool ( newPool, Config (..)
#if defined(METRICS)
                     , withMetrics
#endif
                     ) where

data Config = Config { configHost :: HostName, configPort :: PortNumber
#if defined(METRICS)
  , configSink :: MetricsSink, configHistogram :: Histogram Double
#endif
  , configIdleTime :: NominalDiffTime }

newPool cfg = createPool (connect (configHost cfg) (configPort cfg)) closeConnection
#if defined(METRICS)
    (configSink cfg)
#endif
    (configIdleTime cfg)
```

which formats to:

```haskell
{-# LANGUAGE CPP #-}

module Database.Pool
  ( newPool,
    Config (..),
#if defined(METRICS)
    withMetrics,
#endif
  )
where

data Config = Config
  { configHost :: HostName,
    configPort :: PortNumber,
#if defined(METRICS)
    configSink :: MetricsSink,
    configHistogram :: Histogram Double,
#endif
    configIdleTime :: NominalDiffTime
  }

newPool cfg =
  createPool
    (connect (configHost cfg) (configPort cfg))
    closeConnection
#if defined(METRICS)
    (configSink cfg)
#endif
    (configIdleTime cfg)
```

Look at what had to happen here. The author wrote the record with *leading*
commas, and one of those commas lived inside the conditional. Tilia writes
trailing commas, so every comma had to move to the other side of the field
it punctuates—which means moving them across the conditional boundary. The
comma that now follows `configPort :: PortNumber` sits *outside* the `#if`,
and `configHistogram :: Histogram Double` gained one *inside* it.

The application at the bottom is the other half of the trick. It was written
flat, on one line, with a conditional argument in the middle of it; it comes
out one argument per line, and the conditional argument keeps its place in
the sequence. What is worth noticing is that this is not one layout decision
but two. Tilia laid out a four-argument application and a three-argument one
separately, and what you see is what survived merging the two results. They
happened to agree, so a single copy is written and the directive ends up
around the one argument they differ by.

So, what we take from the choice calculus here is the representation—a
document is a tree that may hold an n-ary *choice* node, standing for
several alternatives at once—together with the equational laws that govern
such trees, which is what tells the merge what it is aiming at:

* a choice all of whose alternatives are the same is not a choice at all
  (D⟨a, a⟩ = a), which is what collapses everything a conditional does not
  touch;
* object structure factors out of a choice (D⟨f a, f b⟩ = f D⟨a, b⟩), which
  is what keeps a conditional around the declaration it was written around
  instead of around the whole module;
* choices in independent dimensions commute, which is what lets the layout
  variants a printer builds and the conditionals an author wrote pass
  through each other rather than multiply.

What we do not take is the rest of the calculus. There are no dimensions and
no binders: a guard is opaque text that is copied into the output and never
read, so two conditionals asking the same question are not tied together,
and nothing ever reasons about which configurations are feasible. Nor does
the variation live in the syntax tree, as it does in the choice calculus and
in the variability-aware parsers built on it—the syntax tree is
`ghc-lib-parser`'s and cannot be changed. The choices live only in the
document Tilia prints from.

CPP support is exciting, but I also need to be honest with you—this is the
most fragile part of the project, so it will be receiving most of my
attention in the next releases.

## Conclusion

The first release of Tilia is on Hackage. Please give it a try and let me
know if it works for you. The home of the project is [this GitHub
repository][github-repo] and that's where you can report all the bugs you
find! A [GitHub action][github-action] is also available.

[github-repo]: https://github.com/mrkkrp/tilia
[github-action]: https://github.com/mrkkrp/setup-tilia
[superc]: https://paulgazzillo.com/papers/pldi12.pdf
[typechef]: https://pl.cs.uni-tuebingen.de/publications/kaestner11variability-aware/
[choice-calculus]: https://web.engr.oregonstate.edu/~erwig/papers/ChoiceCalculus_TOSEM11.pdf
