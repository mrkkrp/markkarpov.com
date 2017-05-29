---
title: Announcing Megaparsec 5
author: Mark Karpov
description: Finally Megaparsec 5 is out, what's new?
published: May 15, 2016
---

I'm happy to announce new major release of Megaparsec.

* [Megaparsec 5 on Hackage](https://hackage.haskell.org/package/megaparsec)
* [GitHub repo](https://github.com/mrkkrp/megaparsec)
* [Site](https://mrkkrp.github.io/megaparsec/)
* [Changelog](https://github.com/mrkkrp/megaparsec/blob/master/CHANGELOG.md)

## Thanks

It's hard not to see that Megaparsec is moving away from its granddaddy
Parsec in every new release. In version five we re-thought some design
decisions made in Parsec and the result is a library written in more
flexible and modern Haskell.

Megaparsec development is completely issue-driven. Actual users and their
practical problems have been shaping the library for quite some time now and
so I would like to thank people who opened most important issues and
feature-requests that made me think again about design of the library:

* [Matteo Ferrando](https://github.com/chamini2), an active user who noticed
  problem with non-flexible position-advancing mechanism.

* [Herbert Valerio Riedel](https://github.com/hvr) who proposed to add
  support for include files and move position-advancing functions into
  `Stream` type class.

* [Wojciech Daniło](https://github.com/wdanilo) who opened issue about
  well-typed and customizable error messages and actively participated in
  discussion.

* [Joe Hermaszewski](https://github.com/expipiplus1) who proposed deriving
  of `Read` and `Show` instances and using special functions for
  pretty-printing.

* And others who proposed and in some cases implemented various minor
  changes.

## Well-typed and customizable error messages

Perhaps the most important feature in this release is well-typed and
customizable error messages. `ParseError` is now parametrized over token
type `t` and custom error component `e`:

```haskell
data ParseError t e = ParseError
  { errorPos        :: NonEmpty SourcePos -- ^ Stack of source positions
  , errorUnexpected :: Set (ErrorItem t)  -- ^ Unexpected items
  , errorExpected   :: Set (ErrorItem t)  -- ^ Expected items
  , errorCustom     :: Set e              -- ^ Associated data, if any
  } deriving (Show, Read, Eq, Typeable)
```

`ErrorItem` is parametrized over token type `t` and looks like this:

```haskell
data ErrorItem t
  = Tokens (NonEmpty t)      -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input
  deriving (Show, Read, Eq, Ord, Data, Typeable)
```

Now error in Megaparsec is not just a bunch of strings, but something more
comprehensible. I expect some people may not like the intensive use of
`NonEmpty`, but I tried to use types that are inhabited only by logically
valid values.

As you can see parse errors support *stack* of source positions. This has
corresonding helpers in `Text.Megaparsec.Prim` and can be used to work with
include files. Pretty-printing function `parseErrorPretty` also knows how to
display stacks of source positions and there are `pushPosition` and
`popPosition` functions available as well.

What about `errorCustom`? You can use your own type there and the whole
library will work with that just fine. We have `Dec` out-of-box:

```haskell
data Dec
  = DecFail String         -- ^ 'fail' has been used in parser monad
  | DecIndentation Ordering Pos Pos -- ^ Incorrect indentation error
  deriving (Show, Read, Eq, Ord, Data, Typeable)
```

`Dec` stands for “default error component” and unless you're doing something
advanced it should just work. To use your own type you just need to make it
instance of `ErrorComponent` and `ShowErrorComponent` (only needed if you
want to pretty-print error messages):

```haskell
class Ord e => ErrorComponent e where

  -- | Represent message passed to 'fail' in parser monad.
  --
  -- @since 5.0.0

  representFail :: String -> e

  -- | Represent information about incorrect indentation.
  --
  -- @since 5.0.0

  representIndentation
    :: Ordering -- ^ Desired ordering between reference level and actual level
    -> Pos             -- ^ Reference indentation level
    -> Pos             -- ^ Actual indentation level
    -> e

class Ord a => ShowErrorComponent a where

  -- | Pretty-print custom data component of 'ParseError'.

  showErrorComponent :: a -> String
```

Then you build on top of `failure` primitive to report error messages with
your data:

```haskell
failure :: MonadParsec e s m
  => Set (ErrorItem (Token s)) -- ^ Unexpected items
  -> Set (ErrorItem (Token s)) -- ^ Expected items
  -> Set e                     -- ^ Custom data
  -> m a
```

For example, indentation-sensitive helpers from `Text.Megaparsec.Lexer` make
use of this:

```haskell
incorrectIndent :: MonadParsec e s m
  => Ordering  -- ^ Desired ordering between reference level and actual level
  -> Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Actual indentation level
  -> m a
incorrectIndent ord ref actual = failure E.empty E.empty (E.singleton x)
  where x = representIndentation ord ref actual
```

Which produces error messages like this (taken from
[this tutorial](https://mrkkrp.github.io/megaparsec/tutorials/indentation-sensitive-parsing.html)):

```
λ> parseTest parser "something\n  one\n    two\n  three"
3:5:
incorrect indentation (got 5, should be equal to 3)
λ> parseTest parser "something\n  one\n  two\n three"
4:2:
incorrect indentation (got 2, should be equal to 3)
λ> parseTest parser "something\n  one\n  two\n  three"
("something",["one","two","three"])
```

Much better than simply “incorrect indentation”!

## The `Stream` type class

The `Stream` type class now has associated type function `Token`:

```haskell
class Ord (Token s) => Stream s where

  -- | Type of token in stream.
  --
  -- @since 5.0.0

  type Token s :: *

  …
```

This is a very handy thing, it allowed to remove one type parameter from
`MonadParsec` type class and write natural type constraints like this:

```haskell
newline :: (MonadParsec e s m, Token s ~ Char) => m Char
newline = char '\n'
```

We will talk about this more in the next section.

Another addition to `Stream` type class is `updatePos` method. Its signature
looks like this:

```haskell
updatePos
  :: Proxy s -- ^ Proxy clarifying stream type ('Token' is not injective)
  -> Pos             -- ^ Tab width
  -> SourcePos       -- ^ Current position
  -> Token s         -- ^ Current token
  -> (SourcePos, SourcePos) -- ^ Actual position and incremented position
```

This improved support for streams of tokens where information about position
of token is encoded in token itself, should be useful with Alex/Happy.

## The `MonadParsec` type class

Declaration of `MonadParsec` changed:

```haskell
class (ErrorComponent e, Stream s, Alternative m, MonadPlus m)
    => MonadParsec e s m | m -> e s where
```

We added type of custom error component `e` and removed type of token `t`
because now it can be found from type of stream `s` via type function
`Token`. This gives us signatures like this:

```haskell
token :: MonadParsec e s m
  => (Token s -> Either ( Set (ErrorItem (Token s))
                        , Set (ErrorItem (Token s))
                        , Set e ) a)
     -- ^ Matching function for the token to parse, it allows to construct
     -- arbitrary error message on failure as well; sets in three-tuple
     -- are: unexpected items, expected items, and custom data pieces
  -> Maybe (Token s) -- ^ Token to report when input stream is empty
  -> m a

tokens :: MonadParsec e s m
  => (Token s -> Token s -> Bool)
     -- ^ Predicate to check equality of tokens
  -> [Token s]
     -- ^ List of tokens to parse
  -> m [Token s]
```

## Support for line folds

Finally support for line folds is here. It's not difficult to implement
though:

```haskell
-- | Create a parser that supports line-folding. The first argument is used
-- to consume white space between components of line fold, thus it /must/
-- consume newlines in order to work properly. The second argument is a
-- callback that receives custom space-consuming parser as argument. This
-- parser should be used after separate components of line fold that can be
-- put on different lines.
--
-- An example should clarify the usage pattern:
--
-- > sc = L.space (void spaceChar) empty empty
-- >
-- > myFold = L.lineFold sc $ \sc' -> do
-- >   L.symbol sc' "foo"
-- >   L.symbol sc' "bar"
-- >   L.symbol sc  "baz" -- for the last symbol we use normal space consumer
--
-- @since 5.0.0

lineFold :: MonadParsec e s m
  => m ()              -- ^ How to consume indentation (white space)
  -> (m () -> m a)     -- ^ Callback that uses provided space-consumer
  -> m a
lineFold sc action =
  sc >> indentLevel >>= action . void . indentGuard sc GT
```

It's super simple and it works.

## Using of `scientific`

Like Attoparsec, we switched to the
[`scientific`](https://hackage.haskell.org/package/scientific) package for
parsing of floating point values in `Text.Megaparsec.Lexer`. The
`Scientific` type is safe against numbers with huge exponents and it can
reliably represent integers too (it even has functions `isFloating`,
`isInteger`, and others that allow handy dispatching), so now we can write
`number` as:

```haskell
-- | Parse a number: either integer or floating point. The parser can handle
-- overlapping grammars graciously. Use functions like
-- 'Data.Scientific.floatingOrInteger' from "Data.Scientific" to test and
-- extract integer or real values.

number :: (MonadParsec e s m, Token s ~ Char) => m Scientific
```

This is amazing, because you can get either floating point number or integer
from it later and `signed` does not need ah-hoc `Signed` type class anymore
to compose with other functions from the module (previously `number`
returned `Either Integer Double`):

```haskell
signed :: (MonadParsec e s m, Token s ~ Char, Num a) => m () -> m a -> m a
```

## Performance

All these new features started to make Megaparsec slower. To counteract this
I had to do some profiling and benchmarking (thanks again to
[Auke Booij](https://github.com/abooij) who contributed benchmarks for
Megaparsec on early stages of development) and indeed some inlining, manual
re-write of `(<*>)`, and careful use of strictness allowed me to improve the
situation. Here are simplified results of comparison on my laptop:

+-------------------------------------+--------------+-----------------+
|Benchmark (size 1000)                | Parsec 3.1.9 | Megaparsec 5.0.0|
+-------------------------------------+--------------+-----------------+
|`string/match`                       | 74.59 μs     | 48.65 μs        |
+-------------------------------------+--------------+-----------------+
|`string/nomatch_early`               | 374.0 ns     | 59.06 ns        |
+-------------------------------------+--------------+-----------------+
|`string/nomatch_late`                | 69.80 μs     | 31.51 μs        |
+-------------------------------------+--------------+-----------------+
|`try-string/match`                   | 76.60 μs     | 48.15 μs        |
+-------------------------------------+--------------+-----------------+
|`try-string/nomatch_early`           | 383.5 ns     | 61.93 ns        |
+-------------------------------------+--------------+-----------------+
|`try-string/nomatch_late`            | 70.88 μs     | 31.72 μs        |
+-------------------------------------+--------------+-----------------+
|`lookahead-string/match`             | 76.78 μs     | 47.07 μs        |
+-------------------------------------+--------------+-----------------+
|`lookahead-string/nomatch_early`     | 389.5 ns     | 60.13 ns        |
+-------------------------------------+--------------+-----------------+
|`lookahead-string/nomatch_late`      | 77.80 μs     | 29.73 μs        |
+-------------------------------------+--------------+-----------------+
|`notfollowedby-string/match`         | 79.46 μs     | 48.54 μs        |
+-------------------------------------+--------------+-----------------+
|`notfollowedby-string/nomatch_early` | 418.8 ns     | 47.17 ns        |
+-------------------------------------+--------------+-----------------+
|`notfollowedby-string/nomatch_late`  | 79.46 μs     | 30.56 μs        |
+-------------------------------------+--------------+-----------------+
|`manual-string`                      | 328.8 μs     | 57.15 μs        |
+-------------------------------------+--------------+-----------------+
|`choice/match`                       | 355.5 μs     | 232.6 μs        |
+-------------------------------------+--------------+-----------------+
|`choice/nomatch`                     | 523.8 μs     | 289.2 μs        |
+-------------------------------------+--------------+-----------------+
|`count`                              | 260.8 μs     | 48.85 μs        |
+-------------------------------------+--------------+-----------------+
|`sepBy1`                             | 357.8 μs     | 100.6 μs        |
+-------------------------------------+--------------+-----------------+
|`manyTill`                           | 464.7 μs     | 139.7 μs        |
+-------------------------------------+--------------+-----------------+

`tokens` (that influences performance of `string`) is coded differently than
in Parsec because I wanted to have a bit different error messages that show
not just current mismatched token, but all sequence parsed up to first
mismatch and all sequence that is expected. `tokens` also backtracks
automatically in Megaparsec 4.3+ but this has no impact on performance.
`choice` is not considerably faster because in Megaparsec it does a lot more
than in Parsec since we need to be able to get parser state on errors for
some features to work.

All in all, given flexibility and features of our fork, the results are not
shameful. To be honest, Parsec can be made faster than Megaparsec with some
effort, but that effort has yet to be applied. I would also appreciate if
someone could add Megaparsec into other parsing benchamarks and let me know
about results.

## To be continued

These are the most important but not all changes and improvements in
Megaparsec 5, please see the
[change log](https://github.com/mrkkrp/megaparsec/blob/master/CHANGELOG.md)
for complete list. I will continue to maintain and improve the library
actively, but do not expect radical changes to happen anytime soon. Now I
would like to concentrate on fixing bugs (should they be discovered) in
timely manner and answering questions. This is partly because I think
Megaparsec 5 already covers pretty much everything I planned for this
project and because I want to spend more time on my other personal pursuits
that have little to do with programming. Your pull requests are still most
welcome though!
