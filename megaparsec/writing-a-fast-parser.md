---
title: Writing a fast parser
desc: Practical recommendations that should help you write a fast parser.
difficulty: 4
date:
  published: September 11, 2016
  updated: September 27, 2017
---

If performance of your Megaparsec parser is worse that you hoped, there may
be ways to improve it. This short guide will instruct you what to attempt,
but you should always check if you're getting better results by profiling
and benchmarking your parsers (that's the only way to understand if you are
doing the right thing when tuning performance).

* If your parser uses a monad stack instead of plain `Parsec` monad (which
  is a monad transformer over `Identity`), make sure you use at least
  version 0.5 of `transformers` library, and at least version 7.0 of
  `megaparsec`. Both libraries have critical performance improvements in
  those versions, so you can just get better performance for free.

* `Parsec` monad will be always faster then `ParsecT`-based monadic stacks.
  Avoid using `StateT`, `WriterT`, and other monad transformers unless
  absolutely necessary. The more you add to the stack, the slower it will
  be.

* Do not keep your parsers polymorphic ~~unless you really have a reason to
  do so~~, just don't. It's best to “fix” the types of parsers specifying
  concrete types, such as `type Parser = Parsec Void Text` for every
  top-level definition. This way GHC will be able to optimize a lot better.

* Keep all your parsing code in single file. If that's not possible, use
  `INLINE` and `INLINEABLE` generously. `INLINE` and `INLINEABLE` pragmas
  make GHC dump functions definitions to interface files and this allows for
  cross-module specialization (I've written a tutorial about this,
  [available
  here](https://www.stackbuilders.com/tutorials/haskell/ghc-optimization-and-fusion/)).

* Use the fast primitives such as `takeWhileP`, `takeWhile1P`, and `takeP`
  whenever you can. [These are
  fast](https://markkarpov.com/post/megaparsec-more-speed-more-power.html#there-is-hope)
  for `Text` and `ByteString`.

* Avoid `oneOf` and `noneOf` preferring `satisfy` and `notChar` whenever
  possible.

A parser can be written in many ways. Think about your grammar and how
parsing happens, when you get some experience with this process, it will be
much easier for you to see how to make your parser faster. Sometimes
however, making a parser faster will also make your code less readable. If
performance of your parser is not a bottleneck in the system you are
building, consider preferring readability over performance.
