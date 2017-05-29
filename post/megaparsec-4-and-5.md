---
title: Megaparsec 4 and 5
author: Mark Karpov
description: What we have achieved with version 4 of Megaparsec and what is planned in version 5.
published: February 23, 2016
---

This post is an attempt to summarize progress made by the Megaparsec project
from its initial release 25 September 2015 to present day and discuss
planned improvements in version 5.

## Questions and answers

Upon release Megaparsec got quite positive reception and it's obviously used
now by people, there are some projects on Hackage that depend on it and
steady feedback of propositions on issue tracker. Of course it didn't
replace Parsec and I doubt it will ever succeed at this, which is OK.

Some people had doubts that are summarized in this list:

1. There are lots of tutorials explaining how to use Parsec. Megaparsec has
   none.

2. How do I know that this library is robust and reliable? Maybe it has bugs
   that have been fixed in Parsec long time ago?

3. What about performance? Is Megaparsec slower than Parsec?

Here are my answers:

1. Megaparsec has a site now, where
   [several tutorials](https://mrkkrp.github.io/megaparsec/tutorials.html)
   are published. This, combined with documentation, which is quite good and
   is kept up-to-date, should be enough to start with most parsing
   tasks. The tutorials will be updated as Megaparsec gets more advanced in
   future versions.

2. I dare to say that Megaparsec can be considered robust and reliable
   now. Since its initial release, only one bug was reported and it was
   fixed on the same day with new version of the library published. We have
   covered 90% of code in our test suite and literally every aspect of
   functioning is checked and controlled. One person told me that they
   switched from Parsec to Megaparsec in production with very good results:
   all tests passed and error messages have become noticeable better.

   I was told by another person that his Megaparsec parser hit
   non-termination bug. He then said that he has no reproducing code
   anymore. I wonder now whether it was programmer's fault or it's a problem
   in Megaparsec? Either way, it does not really matter because even if it's
   a bug in Megaparsec (which I doubt), it will be fixed in timely manner
   once reported.

3. Megaparsec is not slower than Parsec. Even though version 4.4.0 is a bit
   slower than initial release it's still actually *faster* than Parsec. I
   would not emphasize speed, though, because if you need speed, then you
   should probably use `attoparsec`. Megaparsec is about flexibility.

## Evolution of Megaparsec 4

Megaparsec is not just “fixed” Parsec, it's a project that moves on and
aspires to provide robust and simple solution for all common use-cases, take
idea of Parsec to its limits. And it's not just words, in version 4 we have
implemented the following features without accumulating any technical debt
(smoothing of API and minor improvements are omitted):

* Improve access to parser state. You can parse something providing your
  custom initial state (for example you can specify non-standard initial
  textual position) and then extract actual parser state as well as result
  of parsing (or parse error). In some cases, when you parse stream of
  objects (in `many . try`), you can actually parse them as you get input
  in chunks, but make no mistake, incremental parsing, although possible in
  some cases, is just side effect of this new feature, not something
  Megaparsec is designed to provide.

* New `failure` function added by popular request. Now it's possible to
  report more complex custom parsing errors including several error messages
  that can be just rendered or extracted from parse error and
  inspected. Previously users were limited by `fail` and `unexpected` in
  this regard.

* Added native higher-level primitives for indentation-sensitive parsing:
  `indentLevel`, `nonIndented`, and `indentBlock`. These do not require any
  additional state to work. They use only internal state of Megaparsec to
  get current position. Quoting our tutorial:

  > We state that there are top-level items that are not indented
  > (`nonIndented` helps to define parsers for them), and all indented
  > tokens are directly or indirectly are “children” of those top-level
  > definitions. In Megaparsec, we don’t need any additional state to
  > express this. Since all indentation is always relative, our idea is to
  > explicitly tie parsers for “reference” tokens and indented tokens, thus
  > defining indentation-sensitive grammar via pure combination of parsers,
  > just like all the other tools in Megaparsec work. This is different from
  > old solutions built on top of Parsec, where you had to deal with ad-hoc
  > state. It’s also more robust and safer, because the less state you have,
  > the better.

  This works, but parsing of indentation-sensitive grammars is not solved
  completely as of version 4.4.0 because line-folding is still not
  implemented. It will be implemented in version 5.0.0, more about this
  below.

* `tokens` and parsers that are based on it (`string`, `string'`, `symbol`,
  …)  now backtrack automatically. This means you don't need to wrap them in
  `try` anymore. This does not affect performance in any way. This is done
  to match the way parse errors are reported for these combinators, because
  as user gets more control with new features like `withRecovery` it's
  important that position in error message matches actual consumption of
  input.

* `withRecovery` primitive parser was added. This allows to recover from
  parsing errors “on-the-fly” and continue parsing. The errors are not lost,
  you can get them when parsing is finished or even ignore. Read how to use
  it [in our tutorial](https://mrkkrp.github.io/megaparsec/tutorials/fun-with-the-recovery-feature.html).

## What to expect from Megaparsec 5?

We will continue to polish the project. I'm still enthusiastic and thankful
to all people who propose new ideas. Version 5 will be incompatible in some
things, but switching will be trivial. And here is a list of changes that
are planned (it will grow and change, of course):

* Improved error messages for indentation-sensitive parsers. Instead of
  “incorrect indentation” phrase, errors will indicate if it's too small or
  excessive and if precise required indentation is known, it will be
  displayed, like “incorrect indentation (needed 5, but got 7)”.

* Above-mentioned line-folds will be implemented.

* Functions to advance textual position for given token will be moved from
  arguments of `token` and `tokens` to `Stream` type class.

* These functions will be so flexible that users who wish to parse streams
  of tokens produced by `alex` or `happy` will have no trouble at all with
  “syncing” of error positions or anything else.

* Support for include files. When advancing error position it will be
  possible to “switch context” to another file and back. When parse errors
  are reported, stack of files will be shown. How this will affect
  performance needs to be analyzed.

* Better typing may happen with more active use of semigroups and `Natural`
  numbers for things that cannot be negative. This is stimulated by
  inclusion of `semigroups` in `base-4.9.0.0`.

* Some people think that printing a line with `^^^^` showing where parse
  error happened somehow makes for better error messages. Well, it's trivial
  to implement as an (optional) utility.

If you would like to propose something incompatible but cool,
[now is the time](https://github.com/mrkkrp/megaparsec/issues).
