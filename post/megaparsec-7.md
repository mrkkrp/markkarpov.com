---
title: Megaparsec 7
desc: A post about the most important and exiciting changes in Megaparsec 7.
date:
  published: August 27, 2018
---

For a while now I've been working on Megaparsec 7. Due to the fact that my
schedule is more saturated these days, the work hasn't been progressing as
quickly as I expected, but nevertheless I tried to spend my rare free hours
on advancing it, and finally I can say that Megaparsec 7 is close to
release.

The post is about the most obvious things a user will run into when
upgrading. It does not attempt to walk through all the changes, for that
there is a [detailed changelog][changelog] available. Thus, we will talk
about breaking changes and new ways of doing certain things. Finally, there
a bit of benchmarking bravura, because yes, we're now faster than ever
(sometimes a bit faster than Attoparsec).

## Simple changes

The good but boring changes you need to know are the following…

### `parser-combinators` grows, `megaparsec` shrinks

Megaparsec always contained quite a bit of code that could work with any
Parsec-like library. I felt like a shame not to make it available for other
packages to use. So, some time ago I started the
[`parser-combinators`][parser-combinators] package which provides common
parsing commbinators that work with any instance of `Applicative`,
`Alternative`, `Monad`. It's quite general and depends virtually only on
`base`. Recently I included the code to do parsing of permutation phrases
and expressions, so we're now able to drop `Text.Megaparsec.Perm` and
`Text.Megaparsec.Expr` from Megaparsec itself:

* `Text.Megaparsec.Perm` → `Control.Applicative.Permutations`
* `Text.Megaparsec.Expr` → `Control.Monad.Combinators.Expr`

This actually means that you can use these modules with e.g. Attoparsec (I
haven't tried though). I think it's pretty cool.

### General combinators have been moved

There were a few combinators in `Text.Megaparec.Char` and
`Text.Megaparsec.Byte` that are actually not specific to input stream type
and should live in the `Text.Megaparsec` module. So they have been moved.
And renamed.

* Now there is the `single` combinator that is a generalization of `char`
  for arbitrary streams. `Text.Megaparsec.Char` and `Text.Megaparsec.Byte`
  still contain `char` as type-constrained versions of `single`.

* Similarly, now there is the `chunk` combinator that is a generalization of
  `string` for arbitrary streams. The `string` combinator is still
  re-exported from `Text.Megaparsec.Char` and `Text.Megaparsec.Byte` for
  compatibility.

* `satisfy` does not depend on type of token, and so it now lives in
  `Text.Megaparsec`.

* `anyChar` was renamed to `anySingle` and moved to `Text.Megaparsec`.

* `notChar` was renamed to `anySingleBut` and moved to `Text.Megaparsec`.

* `oneOf` and `noneOf` were moved to `Text.Megaparsec`.

## Parse errors story

Megaparsec 6 added the ability to display offending line from original input
stream when pretty-printing parse errors. That's good, but the design has
always felt as an afterthought to me:

* There are *three* functions to pretty-print a `ParseError`:
  `parseErrorPretty`, `parseErrorPretty'`, and `parseErrorPretty_`. The last
  was added because `parseErrorPretty'` actually doesn't allow specifying
  tab width which is necessary to know for proper displaying of lines with
  tabs.

* The functions that try to display the relevant line from input stream
  require the input stream to be passed to them. Having to keep input stream
  around just to be able to display nice error messages is a bit
  inconvenient. In one package I even had to define a product of
  `ParseError` and `Text` to work around this.

* I think [mmark][mmark] is a nice example of what Megaparsec can do. But it
  also showed the limitations of the parsing library. `mmark` can report
  several `ParseError`s at once, and when they are pretty-printed, we
  display the offending line per error from the original input stream. If we
  just use the functions that are provided out-of-the-box, we'll be
  traversing the input stream N times, where N is the number of
  `ParseError`s we want to display. Not nice at all!

It looks like we want:

* A bundle type `ParseErrorBundle` that functions like `parse` will return.

* The type should include everything that is necessary to pretty-print a
  parse error: tab width, input stream to use, etc.

* There will be only one function to pretty print such a bundle, let's call
  it `errorBundlePretty`.

* The bundle should be able to contain several `ParseError`s which are
  sorted. During pretty-printing it should traverse input stream only once.

So here we go:

```haskell
-- | A non-empty collection of 'ParseError's equipped with 'PosState' that
-- allows to pretty-print the errors efficiently and correctly.

data ParseErrorBundle s e = ParseErrorBundle
  { bundleErrors :: NonEmpty (ParseError s e)
    -- ^ A /sorted/ collection of 'ParseError's to display
  , bundlePosState :: PosState s
    -- ^ State that is used for line\/column calculation
  } deriving (Generic)
```

`PosState` is defined like so:

```haskell
-- | Special kind of state that is used to calculate line\/column positions
-- on demand.

data PosState s = PosState
  { pstateInput :: s
    -- ^ The rest of input to process
  , pstateOffset :: !Int
    -- ^ Offset corresponding to beginning of 'pstateInput'
  , pstateSourcePos :: !SourcePos
    -- ^ Source position corresponding to beginning of 'pstateInput'
  , pstateTabWidth :: Pos
    -- ^ Tab width to use for column calculation
  , pstateLinePrefix :: String
    -- ^ Prefix to prepend to offending line (out of scope for this post)
  } deriving (Show, Eq, Data, Typeable, Generic)
```

This is a helper data type that allows to get from one `ParseError` to
another and pretty print them in one pass. Functions like `runParser` or
`parse` always return only one `ParseError` in a bundle, but we can add more
ourselves, which is what I think `mmark` will be doing.

There is a but more about `PosState` though, and it has to do with the
performance improvement in Megaparsec 7.

## Performance improvements

I was thinking how to make Megaparsec 7 faster and simpler. One thing I did
is [dropping stacks of source positions][drop-stacks], which felt good, but
not enough. So I figured: updating `SourcePos` in `State` is expensive, but
pretty much a useless thing to do if a parser doesn't fail.

Why is it useless?

* We only care about `SourcePos` when we want to present `ParseErrors` to
  humans. For everything else a simple `Int` offset as the number of
  consumed tokens so far is perfect.

* Given input stream and things like tab width, an offset determines
  uniquely the corresponding `SourcePos` anyway, so keeping
  `stateTokensProcessed` and `statePos` at the same time is a waste.

* We already traverse input stream when we pretty-print parse errors. We
  could at the same time calculate `SourcePos` from offsets while doing
  that.

So that's the idea:

* Store `Int` offset instead of `SourcePos` position in `ParseError`s.

* Infer `SourcePos` when necessary on pretty-printing.

Guess what, this gives about 100% of speed-up on microbenchmarks (not on all
of them, but on many, and that's impressive), and this does transform into
performance improvements for real parsers too.

I maintain a few projects that use Megaparsec and they all have benchmarks
for the parsers, so I checked how the dev version of Megaparsec is doing:

* For parsers that use `getSourcePos` (`getPosition` in older versions of
  Megaparsec) i.e. lookup column position or deal with indentation there is
  no difference.

* For other parsers there is 20-30% of speedup (memory usage stays mostly
  the same).

Finally, here is the [older benchmark][parsers-bench] comparing Attoparsec
and Megaparsec. I used it to compare Attoparsec vs Megaparsec 6 vs
Megaparsec 7. Here is a table which shows simplified results (run on my
laptop):

| Benchmark | Attoparsec 0.13.2.2 | Megaparsec 6.5.0 | Megaparsec 7.0.0
|-----------|--------------------:|-----------------:|----------------:
| CSV (40)  | 99.62 μs            | 137.2 μs         | 82.75 μs
| Log (40)  | 429.4 μs            | 577.4 μs         | 453.8 μs
| JSON (40) | 27.01 μs            | 48.81 μs         | 33.68 μs

Notably, Megparsec 7 beats Attoparec on the CSV benchmark now. It's written
quite naively of course, if I remember correctly I stole it from some
Attoparsec or Parsec tutorial, but still it demonstrates that the machinery
in the foundation of the library is getting quite speedy.

Memory (showing allocations because max residency is constant and quite low
in all cases):

| Benchmark | Attoparsec 0.13.2.2 | Megaparsec 6.5.0 | Megaparsec 7.0.0
|-----------|--------------------:|-----------------:|----------------:
| CSV (40)  | 397,952             | 557,312          | 357,208
| Log (40)  | 1,181,120           | 1,485,776        | 1,246,496
| JSON (40) | 132,488             | 233,328          | 203,824

Now you probably understand the temptation. But there was also the
conservative part of me which said: “but hey, people are going to want to
get source position from a working parser to attach it to AST or something,
and what about indentation-sensitive parsing which needs to know column
numbers…”.

Hell, that's right. But we're not going to let that spoil the party, are we?

We could always calculate `SourcePos` incrementally and on demand. Re-using
`PosState` we plug it into parser `State`:

```haskell
data State s = State
  { stateInput :: s
    -- ^ The rest of input to process
  , stateOffset :: {-# UNPACK #-} !Int
    -- ^ Number of processed tokens so far
  , statePosState :: PosState s
    -- ^ State that is used for line\/column calculation
  } deriving (Show, Eq, Data, Typeable, Generic)
```

Exploiting the fact that we can only move forward in input stream, we can
write:

```haskell
getSourcePos :: MonadParsec e s m => m SourcePos
getSourcePos = do
  st <- getParserState
  -- We're not interested in the line at which the offset is located in
  -- this case, but the same 'reachOffset' function is used in
  -- 'errorBundlePretty'.
  let (pos, _, pst) = reachOffset (stateOffset st) (statePosState st)
  setParserState st { statePosState = pst }
  return pos
```

Where `reachOffset` is a new method of `Stream` that replaces all the old
methods that had to do with keeping track of source position. At the same
time `reachOffset` fetches `String` representation of the right line in
input to show in parse errors. And it's tuned to be incremental, so only
not-previously-traversed part of input will be processed. I have confirmed
on projects like `mmark` that even if you use `getSourcePos`, there is no
performance regressions, performance stays the same in that case (that's if
you don't call `getSourcePos` on every token, which is a bad idea).

## Conclusion

I think that these two changes (parse error bundles and using offsets)
complement each other rather well and make the library a lot nicer.

Let me know what you think. It'll take some time to finish up the whole
thing, so if you have a concern about the changes I described, please tell
me about it. Once again, the full changelog (so far) is [here][changelog].

[changelog]: https://github.com/mrkkrp/megaparsec/blob/master/CHANGELOG.md
[parser-combinators]: https://hackage.haskell.org/package/parser-combinators
[int-index]: https://int-index.com/
[mmark]: https://hackage.haskell.org/package/mmark
[drop-stacks]: https://github.com/mrkkrp/megaparsec/commit/7120bae9b27f4367ea802935217f71a78c3c2756
[parsers-bench]: https://github.com/mrkkrp/parsers-bench
