---
title: Evolution of error messages
desc: The second and final post about Megaparsec 6 development. This time we walk through re-design of parse error messages.
date:
  published: July 26, 2017
---

This is the second and final post about Megaparsec 6.0.0 development. This
time we are going to walk through re-design of parse error messages.

## Custom error components

Since version 5 we've had the ability to use any data type as custom
component of error message, `e` in `MonadParsec e s m`. So everything that
isn't a combination of expected/unexpected items should be representable as
`e`, which must be an instance of the `ErrorComponent` type class:

```haskell
class Ord e => ErrorComponent e where

  -- | Represent the message passed to 'fail' in parser monad.

  representFail :: String -> e

  -- | Represent information about incorrect indentation.

  representIndentation
    :: Ordering -- ^ Desired ordering between reference level and actual level
    -> Pos             -- ^ Reference indentation level
    -> Pos             -- ^ Actual indentation level
    -> e
```

Its methods force the programmer to provide a way to encode the two built-in
custom error conditions: `fail` usage and incorrect indentation messages.

By default we had `Dec`:

```haskell
-- | “Default error component”. This is our instance of 'ErrorComponent'
-- provided out-of-box.

data Dec
  = DecFail String         -- ^ 'fail' has been used in parser monad
  | DecIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  deriving (Show, Read, Eq, Ord, Data, Typeable)
```

But if you wanted to add something different, there were two options:

* Duplicate the `DecFail` and `DecIndentation` constructors and also copy
  the rendering logic in the instance of `ShowErrorComponent` for your new
  data type. For example, you could define a data type with constructors
  `MyFail`, `MyIndentation`, and `MyCustomThing`.

* Embed `Dec` into your own custom component and delegate all the work to it
  in the `fail` and indentation cases.

After a while I realized that the design sucks because of the boilerplate
one had to deal with to support the obligatory `fail` and indentation
errors, so I wanted to finally fix it in version 6.

After some thinking I came up with the idea of the `ErrorFancy` sum type:

```haskell
-- | Additional error data, extendable by user. When no custom data is
-- necessary, the type is typically indexed by 'Void' to “cancel” the
-- 'ErrorCustom' constructor.

data ErrorFancy e
  = ErrorFail String
    -- ^ 'fail' has been used in parser monad
  | ErrorIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  | ErrorCustom e
    -- ^ Custom error data, can be conveniently disabled by indexing
    -- 'ErrorFancy' by 'Void'
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)
```

It covers the default custom error messages and provides the opportunity to
add something custom via the `ErrorCustom` constructor. By default we don't
want anything custom, so we can just “multiply it by zero” by indexing the
type by `Void`.

Then we have the really straightforward instance definition for
`ShowErrorComponent`:

```haskell
instance ShowErrorComponent e => ShowErrorComponent (ErrorFancy e) where
  showErrorComponent (ErrorFail msg) = msg
  showErrorComponent (ErrorIndentation ord ref actual) =
    "incorrect indentation (got " <> show (unPos actual) <>
    ", should be " <> p <> show (unPos ref) <> ")"
    where
      p = case ord of
            LT -> "less than "
            EQ -> "equal to "
            GT -> "greater than "
  showErrorComponent (ErrorCustom a) = showErrorComponent a

instance ShowErrorComponent Void where
  showErrorComponent = absurd
```

The `ErrorComponent` type class can be dropped now. The user only needs to
think about the custom part—the problem is solved.

## Separation of custom and trivial error messages

Roman Cheplyaka
opened [an issue](https://github.com/mrkkrp/megaparsec/issues/208) which
made me think about re-organizing the `ParseError` data type. Previously we
had:

```haskell
data ParseError t e = ParseError
  { errorPos        :: NonEmpty SourcePos -- ^ Stack of source positions
  , errorUnexpected :: Set (ErrorItem t)  -- ^ Unexpected items
  , errorExpected   :: Set (ErrorItem t)  -- ^ Expected items
  , errorCustom     :: Set e              -- ^ Associated data, if any
  } deriving (Show, Read, Eq, Data, Typeable, Generic)
```

The problem with this is that it by default Megaparsec's machinery generates
errors with unexpected/expected items and sometimes it may get mixed with
custom messages the user tries to generate. See the linked issue for an
example of this, I'll just state here that it's rarely useful to mix these
two types of error messages together, so we want to have two different
constructors: one for trivial errors (unexpected/expected items) and another
one for “fancy” stuff (recall the `ErrorFancy` type from the previous
section):

```haskell
data ParseError t e
  = TrivialError
      (NonEmpty SourcePos)
      (Maybe (ErrorItem t))
      (Set (ErrorItem t))
    -- ^ Trivial errors, generated by Megaparsec's machinery. The data
    -- constructor includes the stack of source positions, unexpected token
    -- (if any), and expected tokens.
  | FancyError (NonEmpty SourcePos) (Set (ErrorFancy e))
    -- ^ Fancy, custom errors.
  deriving (Show, Read, Eq, Data, Typeable, Generic)
```

When merging errors from several branches of parsing fancy errors are
preferred. The merging logic is like this:

```haskell
mergeError :: (Ord t, Ord e)
  => ParseError t e
  -> ParseError t e
  -> ParseError t e
mergeError e1 e2 =
  case errorPos e1 `compare` errorPos e2 of
    LT -> e2
    EQ ->
      case (e1, e2) of
        (TrivialError s1 u1 p1, TrivialError _ u2 p2) ->
          TrivialError s1 (E.union u1 u2) (E.union p1 p2)
        (FancyError {}, TrivialError {}) -> e1
        (TrivialError {}, FancyError {}) -> e2
        (FancyError s1 x1, FancyError _ x2) ->
          FancyError s1 (E.union x1 x2)
    GT -> e1
```

The change also entailed addition of the `fancyFailure` method, the old
`failure` method now can only be used to signal trivial parse errors:

```haskell
class (Stream s, A.Alternative m, MonadPlus m)
    => MonadParsec e s m | m -> e s where

  -- | The most general way to stop parsing and report a trivial
  -- 'ParseError'.

  failure
    :: Maybe (ErrorItem (Token s)) -- ^ Unexpected item (if any)
    -> Set (ErrorItem (Token s)) -- ^ Expected items
    -> m a

  -- | The most general way to stop parsing and report a fancy 'ParseError'.

  fancyFailure
    :: Set (ErrorFancy e) -- ^ Fancy error components
    -> m a
```

I think this is a step forward because we now have a more structured
approach where different types of parse errors do not get mixed resulting in
something that may make no sense. The new approach is not less powerful
because we can plug any type as `e`, e.g. we can attach context to a trivial
parse error and store the whole thing as a fancy error `ErrorFancy e`,
similar to what I have shown
in
[an older post](https://markkarpov.com/post/latest-additions-to-megaparsec.html).

## The problem with multiple unexpected items

Another issue
Roman
[drew my attention to](https://github.com/mrkkrp/megaparsec/issues/227) is
that we can have a set of unexpected items. Indeed, it's a bit strange to
see something like:

```
1:10:
unexpected "foo" or "bar"
expecting "baz"
```

How it's even possible to run into two different unexpected things at the
same time? You can build something weird on top of `token` though and get a
message like the one shown above, but is it really useful? Does it make
sense?

If you don't do anything weird, there is only one case (that I'm aware of)
when you can get multiple unexpected tokens with Megaparsec 5, looks like
this (this is with `master` as it where when Roman opened the issue):

```haskell
module Main (main) where

import Data.Void
import Text.Megaparsec -- current master
import Text.Megaparsec.Char

type Parser = Parsec Void String

pScheme :: Parser String
pScheme = choice
  [ string "data"
  , string "file"
  , string "ftp"
  , string "https"
  , string "http"
  , string "irc"
  , string "mailto" ]

main :: IO ()
main = parseTest pScheme "dat"
```

Then:

```
1:1:
unexpected "dat" or 'd'
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

Yep, since there are alternatives that do not start with a `d`, we could
indeed claim that `d` by itself is unexpected. This is a consequence of
error reporting as implemented by old `tokens` (before we extended the
`Stream` type class,
as
[described here](https://markkarpov.com/post/megaparsec-more-speed-more-power.html)).
The old `tokens` reports unexpected sequence of tokens up to first mismatch.
First mismatch is different for different alternatives, so it gets merged
into the thing you have just seen.

In principle, I have nothing against having `Maybe (ErrorItem t)` instead of
`Set (ErrorItem t)` as unexpected item in `TrivialError`, but then there are
problems with merging parse errors from different branches of parsing. Look
at the definition of `ErrorItem t`:

```haskell
data ErrorItem t
  = Tokens (NonEmpty t)      -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)
```

When merging two `Maybe (ErrorItem t)` from different branches, should
labels take precedence over sequence of tokens? How unexpected `EndOfInput`
should merge with the other stuff? How two labels should merge? Any
particular logic seemed kind of arbitrary, so at the time I decided to keep
the set-based definition.

The change in `Stream` type class and new, more efficient implementation of
`tokens` forced me to re-think the set-based approach again. For efficiency,
the new `tokens` definition reports unexpected token sequence of the same
length as the chunk we match against. With the old merging strategy and sets
of unexpected tokens we could easily get (if we fed something like
`datzzzzz` as input to that scheme parser):

```
1:1:
unexpected "dat", "datz", "datzz", or "datzzz"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

This is definitely much worse than the previous behavior and completely
unacceptable. The logic of `tokens` could not be improved in a way that I
would find reasonable. After all, the behavior of individual `tokens`-based
parsers is OK, the problem was in the merging logic—it needed an update.

The first attempt looked like this:

```haskell
mergeError :: (Ord t, Ord e)
  => ParseError t e
  -> ParseError t e
  -> ParseError t e
mergeError e1 e2 =
  case errorPos e1 `compare` errorPos e2 of
    LT -> e2
    EQ ->
      case (e1, e2) of
        (TrivialError s1 u1 p1, TrivialError _ u2 p2) ->
          -- u1 and u2 (unexpected items) are still sets
          TrivialError s1 (n u1 u2) (E.union p1 p2)
        (FancyError {}, TrivialError {}) -> e1
        (TrivialError {}, FancyError {}) -> e2
        (FancyError s1 x1, FancyError _ x2) ->
          FancyError s1 (E.union x1 x2)
    GT -> e1
  where
    n = …
```

`n`, which I'm too lazy to re-implement for the sake of just this post, made
sure that from any number of `Tokens` (it's a data constructor of
`ErrorItem`, shown above) only single longest `Tokens` item will survive
after merging. The logic is as following: since we only combine parse errors
happening exactly at the same position in input, then any different `Tokens`
things are inevitably prefixes of the input stream at that point. If they
differ, then they are just prefixes of different length, so we can go ahead
and just pick the longest `Tokens` item and drop the others.

This turned the awkward parse error I showed earlier into this:

```
1:1:
unexpected "datzzz"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

Normalization worked well, but now we got a new problem: `ParseError` no
longer was a lawful `Monoid`! Indeed, if we take a `ParseError` that already
contains two `Tokens` items of different length and `mappend` it with
`mempty` (which has unexpected and expected items just as empty sets), the
normalization kicks in and we get a different thing as output: right/left
identity laws for `Monoid` do not hold.

But really, we would like `ParseError` to be a `Monoid`, very much.

If `ParseError` is just a `Semigroup`, then we can't make our parser an
instance of `Alternative` and `MonadPlus` which are monoids on applicative
functors and monads respectively, i.e. they imply that some sort of identity
exists, for which we need `ParseError`'s `mempty`. If we went ahead with
non-monoid `ParseError`, we would need to pull in something
like [semigroupoids](https://hackage.haskell.org/package/semigroupoids), and
use something
like
[`Alt`](https://hackage.haskell.org/package/semigroupoids-5.2/docs/Data-Functor-Alt.html),
which is a path I don't want to step on with Megaparsec, which should stay
close to “popular” abstractions, be reasonably conventional and light in
terms of dependencies.

The only way to keep normalization and make `ParseError` a proper `Monoid`
is to switch from `Set (ErrorItem t)` to `Maybe (ErrorItem t)` for the
unexpected component of error message—we virtually have no choice!

Returning to the problems with merging, there is some points that should
make the arbitrary merging strategy I have chosen less frightening:

* Choose longest `Tokens` item following the logic I've just described.
* Let `Label`s win over sequences of tokens.
* Let `EndOfInput` win over anything else.

The final solution:

```haskell
mergeError :: (Ord t, Ord e)
  => ParseError t e
  -> ParseError t e
  -> ParseError t e
mergeError e1 e2 =
  case errorPos e1 `compare` errorPos e2 of
    LT -> e2
    EQ ->
      case (e1, e2) of
        (TrivialError s1 u1 p1, TrivialError _ u2 p2) ->
          TrivialError s1 (n u1 u2) (E.union p1 p2)
        (FancyError {}, TrivialError {}) -> e1
        (TrivialError {}, FancyError {}) -> e2
        (FancyError s1 x1, FancyError _ x2) ->
          FancyError s1 (E.union x1 x2)
    GT -> e1
  where
    n (Just x) (Just y) = Just (max x y)
    n x        y        = x <|> y
```

Yep, just like that. In reality there is nothing to worry about. As I said,
two “unexpected” components to merge are inevitably located exactly at the
same spot, so they are essentially different names for the same thing.

## Displaying of offending line in error messages

Finally, we now have the ability to display offending line in error
messages, with problematic position marked with the caret symbol `^`. Parse
errors now look like this:

```
1:10:
  |
1 | foo = (x $ y) * 5 + 7.2 * z
  |          ^
unexpected '$'
expecting ')', operator, or the rest of expression
```

Use `parseErrorPretty'` or `parseTest'` instead of `parseErrorPretty` and
`parseTest` respectively to get the enhanced print-outs.

I'd like to thank Alex Washburn
aka [recursion-ninja](https://github.com/recursion-ninja) for working on the
feature.

## Conclusion

Right now I'm playing with Megaparsec 6 porting some existing parsers and
I'm very pleased with results. I think I'll upload the library on Hackage
very soon (maybe tomorrow, I'll announce
on [Twitter](https://twitter.com/mrkkrp) when done). When version 6 is out
I'll push updates to tutorials and upgrade some projects that I maintain,
such as [`stache`](https://hackage.haskell.org/package/stache).
