---
title: Megaparsec chapter from “Intermediate Haskell”
desc: This is a preview of the chapter about Megaparsec I wrote for the Intermediate Haskell book.
difficulty: 5
date:
  published: December 3, 2017
---

*This is a preview of the chapter about Megaparsec I wrote for the
[Intermediate Haskell](https://intermediatehaskell.com/) book.*

This chapter will present how to use the
[`megaparsec`](https://hackage.haskell.org/package/megaparsec) package,
which is a monadic parser combinator library. Before we start, it seems to
be appropriate to enumerate popular parsing libraries that are available in
the Haskell ecosystem and say a few words about various trade-offs they
make:

* [`parsec`](https://hackage.haskell.org/package/megaparsec) has been the
  “default” parsing library in Haskell for a long time. Parsec focuses on
  quality of error messages. It however does not have good test coverage and
  is currently in maintenance mode.

* [`attoparsec`](https://hackage.haskell.org/package/attoparsec) is a
  robust, fast parsing library with focus on performance. Its downsides are
  poor quality of error messages, inability to be used as a monad
  transformer, and a limited set of types that can be used as input stream.

* [`trifecta`](https://hackage.haskell.org/package/trifecta) features good
  error messages but is under-documented and hard to figure out. It can
  parse `String` and `ByteString` natively, but not `Text`.

* [`Earley`](https://hackage.haskell.org/package/Earley) is a newer kid on
  the block. The library uses the Earley's algorithm which allows to parse
  arbitrary context-free grammars. Earley is slower than Parsec and can't
  parse grammars that are not context-free. It's thus fundamentally less
  powerful than other libraries in the list.

* [`megaparsec`](https://hackage.haskell.org/package/megaparsec) is a fork
  of Parsec that has been actively developed in the last few years. The
  current version strikes a nice balance between speed, quality of error
  messages, and flexibility. As an unofficial successor of Parsec, it stays
  conventional and immediately familiar for users who have used Parsec or
  who have read Parsec tutorials.

We'll be using Megaparsec 6, which by the time this book is published will
probably have replaced the older version 5 almost everywhere.

## The `MonadParsec` type class

All tools in Megaparsec work with any instance of the `MonadParsec` type
class. The type class abstracts *primitive combinators*—elemental building
blocks of all Megaparsec parsers, combinators that can't be expressed via
other combinators. We'll explore all the primitives through this chapter
introducing them gradually to solve various real-world challenges that may
arise during writing of a parser.

This section aims to remind why we have Megaparsec API defined in terms of
the `MonadParsec` type class, instead of following e.g. the Parsec's path of
working with a concrete monad transformer. To understand the motivation,
recall that the order of layers in a monadic stack matters. If we combine
`ReaderT` and `State` like this:

```haskell
type MyStack a = ReaderT MyContext (State MyState) a
```

The outer layer, `ReaderT` can't know anything about its inner monad
`State`. The `Monad` instance for `ReaderT` describes the binding strategy:

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Monad (ReaderT r m) where
  m >>= k = ReaderT $ \r -> do
    a <- runReaderT m r
    runReaderT (k a) r
```

We can see that the `do` block is in the abstract inner monad `m`, so in the
example given above (with `State` inside), state of `m` is passed to `k` via
monadic bind of the monad `m`. There is no way to prevent that, it's what we
typically want from `(>>=)` anyway.

The `(<|>)` method of the `Alternative` type class, which happens to be
useful for expressing the notion of alternatives in parsing libraries of the
Parsec family, works differently—it “splits” state and the two branches of
parsing do not contact anymore, so we get *backtracking state* in the sense
that if the first branch is discarded changes to its state are also
discarded and cannot influence the second branch (we “backtrack” the state
when the first branch fails).

To illustrate, let's see the definition of `Alternative` for `ReaderT`:

```haskell
instance Alternative m => Alternative (ReaderT r m) where
  empty   = liftReaderT empty
  m <|> n = ReaderT $ \r -> runReaderT m r <|> runReaderT n r
```

This all is very nice, because `ReaderT` is a “stateless” monad transformer
and it's easy to delegate the actual work to the inner monad (the
`Alternative` instance of `m` comes in handy here). Since `State s a` is
just a type synonym for `StateT s Identity a`, we should look at the
`Alternative` instance for `StateT s m` itself:

```haskell
instance (Functor m, Alternative m) => Alternative (StateT s m) where
  empty = StateT $ \_ -> empty
  StateT m <|> StateT n = StateT $ \s -> m s <|> n s
```

Here we can see the splitting of state `s`.

What about `ParsecT`? Let's consider now putting `State` inside `ParsecT`
like this:

```haskell
type MyStack a = ParsecT Void Text (State MyState) a
```

Ignore the `Void` and `Text` type parameters, we'll get to them in next
sections. For now, let's point out that `ParsecT` is more complex than
`ReaderT` and its implementation of `(<|>)` has to do more:

* managing of the state of the parser itself;
* merging of parse errors (when appropriate), should they happen.

Implementation of `(<|>)` in `ParsecT`'s instance of `Alternative` thus
cannot delegate its work to the `Alternative` instance of the underlying
monad `State MyState` and so no splitting of state happens—we have no
backtracking.

Let's demonstrate this with an example:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)

type Parser = ParsecT Void Text (State String)

parser0 :: Parser String
parser0 = a <|> b
  where
    a = "foo" <$ put "branch A"
    b = get   <* put "branch B"

parser1 :: Parser String
parser1 = a <|> b
  where
    a = "foo" <$ put "branch A" <* empty
    b = get   <* put "branch B"

main :: IO ()
main = do
  let run p          = runState (runParserT p "" "") "initial"
      (Right a0, s0) = run parser0
      (Right a1, s1) = run parser1

  putStrLn  "Parser 0"
  putStrLn ("Result:      " ++ show a0)
  putStrLn ("Final state: " ++ show s0)

  putStrLn  "Parser 1"
  putStrLn ("Result:      " ++ show a1)
  putStrLn ("Final state: " ++ show s1)
```

Here is the result of running the program:

```
Parser 0
Result:      "foo"
Final state: "branch A"
Parser 1
Result:      "branch A"
Final state: "branch B"
```

With parser 0 we can see that the branch `b` is not even tried. With parser
1 however it's obvious that the final result—the value returned by `get`—
comes from the branch `a` even though it fails because of `empty` and it's
the branch `b` that succeeds (`empty` in the context of parsing means “fail
instantly and without any information about what's happened”). No
backtracking happens.

What to do if we want backtracking custom state in our parser? We can
provide that if we allow to wrap `ParsecT` [emph](inside) `StateT`:

```haskell
type MyStack a = StateT MyState (ParsecT Void Text Identity) a
```

Now if we use `(<|>)` in `MyStack` the instance used is that of `StateT`:

```haskell
StateT m <|> StateT n = StateT $ \s -> m s <|> n s
```

Which gives us backtracking state and then delegates the rest of the work to
`Alternative` instance of its inner monad—`ParsecT`. This behavior is
exactly what we want:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)

type Parser = StateT String (ParsecT Void Text Identity)

parser :: Parser String
parser = a <|> b
  where
    a = "foo" <$ put "branch A" <* empty
    b = get   <* put "branch B"

main :: IO ()
main = do
  let p            = runStateT parser "initial"
      Right (a, s) = runParser p "" ""
  putStrLn ("Result:      " ++ show a)
  putStrLn ("Final state: " ++ show s)
```

The program prints:

```
Result:      "initial"
Final state: "branch B"
```

To make this approach feasible, `StateT` should support the whole set of
primitive parsers, so we can work with it just like with `ParsecT`. In other
words, it should be an instance of `MonadParsec`, just like it is an
instance of not only `MonadState`, but also e.g. `MonadWriter` if its inner
monad is an instance of `MonadWriter` (in MTL):

```haskell
instance MonadWriter w m => MonadWriter w (StateT s m) where …
```

Indeed, we can lift primitives from inner instance of `MonadParsec` into
`StateT`:

```haskell
instance MonadParsec e s m => MonadParsec e s (StateT st m) where …
```

Megaparsec defines instances of `MonadParsec` for all MTL monad transformers
so the user is free to insert the transformers inside of `ParsecT` or wrap
`ParsecT` in those transformers achieving different kinds of interactions
between the layers of monadic stack.

## `ParsecT` and `Parsec`

`ParsecT` is the main parser monad in Megaparsec. All other instances of
`MonadParsec` just lift operations defined for `ParsecT`. `ParsecT e s m a`
is parametrized like this:

* `e` is the type of custom component of error messages. If we don't want
  anything custom (and for now we don't), we just use `Void`.

* `s` is the type of input stream. Megaparsec works out-of-the-box with
  `String`, strict and lazy `Text`, and strict and lazy `ByteString`s. It's
  also possible to work with custom input streams.

* `m` is the inner monad of the `ParsecT` monad transformer.

* `a` is the monadic value.

Since most of the time `m` is nothing but `Identity`, the `Parsec` type
synonym is quite useful:

```haskell
type Parsec e s a = ParsecT e s Identity a
```

`Parsec` is simply the non-transformer version of `ParsecT`.

Speaking of type synonyms, the best way to start writing parser with
Megaparsec is to define a custom type synonym for your parser. This is a
good idea for two reasons:

* It'll be easier to add top level signatures like `Parser Int` where
  `Parser` is your parsing monad. Without the signatures, things like `e`
  will often be ambiguous—it's the flip side of flexible and thus
  polymorphic API of the library.

* Working with concrete types with all type variables fixed helps GHC
  optimize a lot better. GHC can't do much in terms of optimization if your
  parsers stay polymorphic. Although Megaparsec API is polymorphic, it's
  expected that end user will stick to a concrete type of parsing monad, so
  inlining and the fact that most functions have their definition dumped
  into so-called *interface files* will allow GHC produce very efficient
  non-polymorphic code.

Let's define a type synonym (typically called `Parser`) like this:

```haskell
type Parser = Parsec Void Text
--                   ^    ^
--                   |    |
-- Custom error component Type of input stream
```

Unless stated otherwise, when you see `Parser` later in the chapter, assume
this type synonym.

## Character and binary streams

It has been said that Megaparsec can work with five types of input stream
out-of-the-box: `String`, strict and lazy `Text`, and strict and lazy
`ByteString`s. This is possible because the library makes these types
instances of the `Stream` type class which abstracts the functionality that
every data type should support to be used as input to a Megaparsec parser.

We won't go into details about `Stream`, but it's worth noting that the type
class has two type functions associated with it:

* `Token s` for stream `s` is the type of single token. Common examples are
  `Char` and `Word8`, although it may be something else for custom streams.

* `Tokens s` for stream `s` is the type of a “chunk” of stream. The concept
  of *chunk* was only introduced for performance reasons. Indeed, it's often
  possible to have a more efficient representation of part of a stream which
  is isomorphic to list of tokens `[Token s]`. For example, input stream of
  the type `Text` has `Tokens s ~ Text`: chunk of `Text` is just `Text`.

We can put all the default input streams into a single table like this:

| `s`                 | `Token s` | `Tokens s`          |
|---------------------|-----------|---------------------|
| `String`            | `Char`    | `String`            |
| strict `Text`       | `Char`    | strict `Text`       |
| lazy `Text`         | `Char`    | lazy `Text`         |
| strict `ByteString` | `Word8`   | strict `ByteString` |
| lazy `ByteString`   | `Word8`   | lazy `ByteString`   |

It's important to get used to the `Token` and `Tokens` type functions
because they are ubiquitous in the types of Megaparsec API.

You may have noticed that if we group all default input streams by token
type, we'll get two groups:

* character streams, for which `Token s ~ Char`: `String` and strict/lazy
  `Text`,

* binary streams, for which `Token s ~ Word8`: strict and lazy
  `ByteString`s.

It turns out that with Megaparsec it's not necessary to code the same
parsers for every type of input stream (this is the case, for example, with
the Attoparsec library), but still we must have different code for different
token types:

* to get common combinators for character streams, import the
  [`Text.Megaparsec.Char`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html)
  module;

* to get the same for binary streams, import
  [`Text.Megaparsec.Byte`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte.html).

Before we explore the contents of those modules however, we need to
introduce a couple of primitives (i.e. methods of the `MonadParsec` type
class) on which the two modules are built, so we understand the tools we're
going to use.

The first primitive is called `token`, and correspondingly it allows to
parse a `Token s`:

```haskell
token :: MonadParsec e s m
  => (Token s -> Either ( Maybe (ErrorItem (Token s))
                        , Set (ErrorItem (Token s)) )
                        a)
    -- ^ Matching function
  -> Maybe (Token s)
     -- ^ Token to report when input stream is empty
  -> m a
```

The first argument of `token` is the matching function for the token to
parse. It allows to construct an error message with unexpected `Maybe
(ErrorItem (Token s))` and expected `Set (ErrorItem (Token s))` components
on failure, return `Right a` on success.

The second argument allows to specify the unexpected token that will be
reported if input stream is empty. It's necessary because the matching
function won't be called in that case at all but still we would like to
report an unexpected token when we know it in advance.

The second primitive is called `tokens` and it allows to parse `Tokens s`,
that is, it can be used to match a fixed chunk of input:

```haskell
tokens :: MonadParsec e s m
  => (Tokens s -> Tokens s -> Bool)
    -- ^ Predicate to check equality of chunks
  -> Tokens s
    -- ^ Chunk of input to match against
  -> m (Tokens s)
```

To better understand how to use the primitives, let's see some definitions
from the
[`Text.Megaparsec.Char`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html)
module. `satisfy` is a fairly common combinator, you give it a predicate
that returns `True` for tokens we want to match and it gives you a parser
back:

```haskell
satisfy :: MonadParsec e s m
  => (Token s -> Bool) -- ^ Predicate to apply
  -> m (Token s)
satisfy f = token testChar Nothing
  where
    testChar x =
      if f x
        then Right x
        else Left (pure (Tokens (x:|[])), Set.empty)
        --         ^                      ^
        --         |                      |
        --         unexpected item        expected items
```

`Set.empty` comes from
[`Data.Set`](https://hackage.haskell.org/package/containers/docs/Data-Set.html).
`Tokens` on the term level (the last line) has nothing to do with `Tokens`
on the type level, it's just a constructor of the `ErrorItem` type which
we'll explore in details when we'll be discussing parse errors.

`satisfy` should look understandable, let's see how it works. To play with a
parser we need a helper function that would run it. For our purposes
(testing in GHCi), there are `parseTest` and `parseTest'` functions. They
both run a parser on given input, but `parseTest'** also prints the line on
which parse error happened, so we'll prefer `parseTest'**.

**NOTE**: The reason `parseTest'` has not replaced `parseTest` altogether is
that the ability to display the relevant line of input imposes an additional
constraint which may be inconvenient to satisfy for some custom streams. In
addition to that, it's nice to be able to switch off printing of offending
line sometimes.

First, let's start GHCi and import some modules:

```haskell
λ> import Text.Megaparsec
λ> import Text.Megaparsec.Char
λ> import Data.Text (Text)
λ> import Data.Void
```

We add the `Parser` type synonym that we'll use to resolve ambiguity in the
type of the parsers:

```haskell
λ> type Parser = Parsec Void Text
```

We also need to enable the `OverloadedStrings` language extension so we can
use string literals as `Text` values:

```haskell
λ> :set -XOverloadedStrings
λ> parseTest' (satisfy (== 'a') :: Parser Char) ""
1:1:
  |
1 | <empty line>
  | ^
unexpected end of input
λ> parseTest' (satisfy (== 'a') :: Parser Char) "a"
'a'
λ> parseTest' (satisfy (== 'a') :: Parser Char) "b"
1:1:
  |
1 | b
  | ^
unexpected 'b'
λ> parseTest' (satisfy (> 'c') :: Parser Char) "a"
1:1:
  |
1 | a
  | ^
unexpected 'a'
λ> parseTest' (satisfy (> 'c') :: Parser Char) "d"
'd'
```

**NOTE**: The `:: Parser Char` annotation is necessary because `satisfy` by
itself is polymorphic, so `parseTest'` cannot know what to use in place of
`e` and `s` in `MonadParsec e s m` (`m` is assumed to be `Identity` with
these helpers). If we worked with a pre-existing parser which had a type
signature, the explicit clarification of parser type would be unnecessary.

That's it, seems to work all right. The problem with `satisfy` is that it
does not say what is expected when it fails, because we can't analyze the
function which caller of `satisfy` provides. There are other combinators
that are less general, but can generate more helpful error messages instead,
for example `char`, which matches a specific token value:

```haskell
char :: MonadParsec e s m => Token s -> m (Token s)
char c = token testChar (Just c)
  where
    f x = Tokens (x:|[])
    testChar x =
      if x == c
        then Right x
        else Left (pure (f x), Set.singleton (f c))
```

The definition is similar to `satisfy` but instead of empty set of expected
values we got something more concrete:

```haskell
λ> parseTest' (char 'a' :: Parser Char) "b"
1:1:
  |
1 | b
  | ^
unexpected 'b'
expecting 'a'
λ> parseTest' (char 'a' :: Parser Char) "a"
'a'
```

There are also two parsers defined in terms of `tokens`:

```haskell
string :: MonadParsec e s m
  => Tokens s
  -> m (Tokens s)
string = tokens (==)

string' :: (MonadParsec e s m, CI.FoldCase (Tokens s))
  => Tokens s
  -> m (Tokens s)
string' = tokens ((==) `on` CI.mk)
```

They match fixed chunks of input, `string` case-sensitively, while `string'`
case-insensitively. For case-insensitive matching the
[`case-insensitive`](https://hackage.haskell.org/package/case-insensitive)
package is used, thus the `FoldCase` constraint.

Let's try use the new combinators too:

```haskell
λ> parseTest' (string "foo" :: Parser Text) "foo"
"foo"
λ> parseTest' (string "foo" :: Parser Text) "bar"
1:1:
  |
1 | bar
  | ^
unexpected "bar"
expecting "foo"
λ> parseTest' (string' "foo" :: Parser Text) "FOO"
"FOO"
λ> parseTest' (string' "foo" :: Parser Text) "FoO"
"FoO"
λ> parseTest' (string' "foo" :: Parser Text) "FoZ"
1:1:
  |
1 | FoZ
  | ^
unexpected "FoZ"
expecting "foo"
```

OK, we can match a single token and a chunk of input. The next step is to
learn how to combine the building blocks to write more interesting parsers.

## Monadic and applicative syntax

The simplest way to combine parsers is to demand their execution in
succession. `ParsecT` and `Parsec` are monads, and monadic bind is exactly
what we use for sequencing our parsers:

```haskell
mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)
```

We can run it to check that everything works as expected:

```haskell
λ> parseTest' mySequence "abc"
('a','b','c')
λ> parseTest' mySequence "bcd"
1:1:
  |
1 | bcd
  | ^
unexpected 'b'
expecting 'a'
λ> parseTest' mySequence "adc"
1:2:
  |
1 | adc
  |  ^
unexpected 'd'
expecting 'b'
```

An alternative syntax for sequential execution is possible if we remember
that every monad is also an applicative functor, and so we can use
applicative syntax:

```haskell
mySequence :: Parser (Char, Char, Char)
mySequence =
  (,,) <$> char 'a'
       <*> char 'b'
       <*> char 'c'
```

The second version works just like the first. Which style to use is often a
matter of taste. Monadic style is arguably more verbose and sometimes
clearer, while applicative style is often more concise. That said, monadic
style is of course more powerful just because monads are more powerful than
applicative functors.

## Forcing consumption of input with `eof`

However, `Applicative` is often powerful enough to allow doing quite
interesting things. Equipped with an associative operator which has
identity, we get a monoid on applicative functors expressed in Haskell via
the `Alternative` type class. The
[`parser-combinators`](https://hackage.haskell.org/package/parser-combinators)
package provides quite a few abstract combinators built around the concepts
of `Applicative` and `Alternative`. The
[`Text.Megaparsec`](https://hackage.haskell.org/package/megaparsec-6.2.0/docs/Text-Megaparsec.html)
module re-exports them from
[`Control.Applicative.Combinators`](https://hackage.haskell.org/package/parser-combinators-0.2.0/docs/Control-Applicative-Combinators.html).

One of the most common combinators is called `many`. It allows us to run a
given parser *zero* or more times:

```haskell
λ> parseTest' (many (char 'a') :: Parser [Char]) "aaa"
"aaa"
λ> parseTest' (many (char 'a') :: Parser [Char]) "aabbb"
"aa"
```

The second result may be a bit surprising. The parser consumed `a`s that
matched, but stopped after that. Well, we did not say what we want to do
after `many (char 'a')`!

Most of the time we want to actually force a parser to consume entire input,
and report parse errors instead of being shy and stopping silently. This is
done by demanding that we reach end of input. Happily, although end of input
is nothing but a concept, there is a primitive called `eof` that does not
ever consume any input and only succeeds at the end of input. Let's add it
to our parser and try again:

```haskell
λ> parseTest' (many (char 'a') <* eof :: Parser [Char]) "aabbb"
1:3:
  |
1 | aabbb
  |   ^
unexpected 'b'
expecting 'a' or end of input
```

We did not say anything about `b`s in our parser, and they are certainly
unexpected.

## Working with alternatives

From now on we'll be developing a real, useful parser that can parse URIs of
the following form:

```
scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
```

We should remember that things in square brackets `[]` are optional, they
may or may not appear in a valid URI. `[]` may be even nested to express a
possibility inside another possibility. We will handle all of this.

Let's start with `scheme`. We will accept only schemes that are known to us,
such as: `data`, `file`, `ftp`, `http`, `https`, `irc`, and `mailto`.

To match a fixed sequence of characters we use `string`. To express a
choice, we use the `(<|>)` method from the `Alternative` type class. So we
can write:

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

pScheme :: Parser Text
pScheme = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"
```

Let's try it:

```haskell
λ> parseTest' pScheme ""
1:1:
  |
1 | <empty line>
  | ^
unexpected end of input
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
λ> parseTest' pScheme "dat"
1:1:
  |
1 | dat
  | ^
unexpected "dat"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
λ> parseTest' pScheme "file"
"file"
λ> parseTest' pScheme "irc"
"irc"
```

Looks good, but the defintion of `pScheme` is a bit too repetitive. There is
a way to write `pScheme` with the `choice` combinator:

```haskell
pScheme :: Parser Text
pScheme = choice
  [ string "data"
  , string "file"
  , string "ftp"
  , string "http"
  , string "https"
  , string "irc"
  , string "mailto" ]
```

**NOTE**: `choice` is just a synonym for `asum`—an operation that folds a
list putting `(<|>)` between its elements, so the two definitions of
`pScheme` are actually the same, although the one which uses `choice` may
look a bit nicer.

After scheme, there should be a colon `:`. To require something to go after
something else, we use monadic bind or `do`-notation:

```haskell
data Uri = Uri
  { uriScheme :: Text
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  r <- pScheme
  void (char ':')
  return (Uri r)
```

If we try to run `pUri`, we'll see that it requires `:` to follow the scheme
name now:

```haskell
λ> parseTest' pUri "irc"
1:4:
  |
1 | irc
  |    ^
unexpected end of input
expecting ':'
λ> parseTest' pUri "irc:"
Uri {uriScheme = "irc"}
```

We are not done with the scheme parsing though. A good Haskell programmer
tries to define types in such a way so incorrect data simply cannot be
represented. Not every `Text` value is a valid scheme. Let's define a data
type to represent schemes and make our `pScheme` parser return value of that
type:

```haskell
data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttp   <$ string "http"
  , SchemeHttps  <$ string "https"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Uri = Uri
  { uriScheme :: Scheme
  } deriving (Eq, Show)
```

**NOTE**: The `(<$)` operator just puts the value on its left-hand side into
a functorial context replacing whatever is there at the moment. `a <$ f` is
the same as `const a <$> f`, but can be more efficient for some functors.

Let's continue playing with our parser:

```haskell
λ> parseTest' pUri "https:"
1:5:
  |
1 | https:
  |     ^
unexpected 's'
expecting ':'
```

Hmm, `https` should be a valid scheme. Can you figure out what went wrong?
The parser tries the alternatives one by one, and `http` matches, so it does
not go further to try `https`. The solution is to put the `SchemeHttps <$
string "https"` line before the `SchemeHttp <$ string "http"` line.
Remember: *with alternatives, order matters*!

Now `pUri` works correctly:

```haskell
λ> parseTest' pUri "http:"
Uri {uriScheme = SchemeHttp}
λ> parseTest' pUri "https:"
Uri {uriScheme = SchemeHttps}
λ> parseTest' pUri "mailto:"
Uri {uriScheme = SchemeMailto}
λ> parseTest' pUri "foo:"
1:1:
  |
1 | foo:
  | ^
unexpected "foo:"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

## Controlling backtracking with `try`

The next part to handle is `[//[user:password@]host[:port]]`—authority. Here
we have nested optional parts, let's update the `Uri` type to reflect this:

```haskell
data Uri = Uri
  { uriScheme    :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)
```

Now we need to discuss an important concept called *backtracking*. We have
already mentioned it in connection with custom state and it's quite the same
for parser's internal state as well. Backtracking is a way to travel back in
time “un-consuming” input in the process. As we have already seen this is
important primarily with branching. Here is an example:

```haskell
alternatives :: Parser (Char, Char)
alternatives = foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'
```

Looks reasonable, let's try it:

```haskell
λ> parseTest' alternatives "ab"
('a','b')
λ> parseTest' alternatives "ac"
1:2:
  |
1 | ac
  |  ^
unexpected 'c'
expecting 'b'
```

What happens here is that `char 'a'` part of `foo` (which is tried first)
succeeded and consumed an `a` from the input stream. `char 'b'` then failed
to match against `c` and so we ended up with this error. An important detail
here is `(<|>)` did not even try `bar` because `foo` has consumed some
input!

This is done for performance reasons and because it would make no sense to
run `bar` feeding it leftovers of `foo` anyway. `bar` wants to be run from
the same point in input stream as `foo`. Megaparsec does not go back
automatically, unlike for example Attoparsec, so we must use a primitive
called `try` to express our wish to backtrack explicitly. `try p` makes it
so that if `p` fails consuming input, `try p` fails as if no input has been
consumed (in fact, it backtracks the entire parser state). This allows
`(<|>)` to try its right-hand alternative:

```haskell
alternatives :: Parser (Char, Char)
alternatives = try foo <|> bar
  where
    foo = (,) <$> char 'a' <*> char 'b'
    bar = (,) <$> char 'a' <*> char 'c'
```

```haskell
λ> parseTest' alternatives "ac"
('a','c')
```

All primitives that actually consume input (there are also primitives that
alter behavior of existing parsers, such as `try` itself) are “atomic” in
terms of input consumption. This means that if they fail, they backtrack
automatically, so there is no way they can consume some input and then fail
halfway through. This is why `pScheme` with its list of alternatives works:
`string` is defined on top of `tokens` and `tokens` is a primitive. We
either match the entire string with `string` or we fail without consuming
input stream at all.

Back to parsing URIs, `(<|>)` can be used to build a handy combinator called
`optional`:

```haskell
optional :: Alternative f => f a -> f (Maybe a)
optional p = (Just <$> p) <|> pure Nothing
```

If `p` in `optional p` matches, we get its result in `Just`, otherwise
`Nothing` is returned. Just what we want! There is no need to define
`optional`,
[`Text.Megapasec`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html)
re-exports this combinator for us. We can now use it in `pUri`:

```haskell
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme
  void (char ':')
  uriAuthority <- optional . try $ do            -- (1)
    void (string "//")
    authUser <- optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar       -- (3)
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal) -- (4)
    return Authority {..}                        -- (5)
  return Uri {..}                                -- (6)
```

**NOTE**: I took the liberty of accepting any alpha-numeric sequences of
characters as username and password, and made similarly arbitrary
simplifications in the format of host for the sake of simplicity.

Important points here:

1. We need to wrap the argument of `optional` with `try` because it's a
   composite parser, not a primitive.

2. The same as 1.

3. `some` is just like `many`, but demands that its argument parser matches
   at least once: `some p = (:) <$> p <*> many p`.

4. Do not use `try` unless necessary! Here if `char ':'` succeeds (which is
   by itself built on top of `token`, so it does not need a `try`), we know
   for sure that port must follow after it, so we just demand a decimal
   number with `L.decimal`. After matching `:` we're sort of committed and
   do not need a way to go back.

5. This assembles an `Authority` value using the `RecordWildCards` languge
   extension.

6. The same as 5.

Play with `pUri` in GHCi and see for yourself that it works:

```haskell
λ> parseTest' (pUri <* eof) "https://mark:secret@example.com"
Uri
  { uriScheme = SchemeHttps
  , uriAuthority = Just (Authority
    { authUser = Just ("mark","secret")
    , authHost = "example.com"
    , authPort = Nothing } ) }
λ> parseTest' (pUri <* eof) "https://mark:secret@example.com:123"
Uri
  { uriScheme = SchemeHttps
  , uriAuthority = Just (Authority
    { authUser = Just ("mark","secret")
    , authHost = "example.com"
    , authPort = Just 123 } ) }
λ> parseTest' (pUri <* eof) "https://example.com:123"
Uri
  { uriScheme = SchemeHttps
  , uriAuthority = Just (Authority
    { authUser = Nothing
    , authHost = "example.com"
    , authPort = Just 123 } ) }
λ> parseTest' (pUri <* eof) "https://mark@example.com:123"
1:13:
  |
1 | https://mark@example.com:123
  |             ^
unexpected '@'
expecting '.', ':', alphanumeric character, or end of input
```

## Debugging parsers

However, you may find that there is some funny stuff going on:

```haskell
λ> parseTest' (pUri <* eof) "https://mark:@example.com"
1:7:
  |
1 | https://mark:@example.com
  |       ^
unexpected '/'
expecting end of input
```

Certainly the parse error could be better! What to do? The easiest way to
figure out what's going on is to use the built-in `dbg` helper:

```haskell
dbg :: (Stream s, ShowToken (Token s), ShowErrorComponent e, Show a)
  => String            -- ^ Debugging label
  -> ParsecT e s m a   -- ^ Parser to debug
  -> ParsecT e s m a   -- ^ Parser that prints debugging messages
```

Let's use it in `pUri`:

```haskell
pUri :: Parser Uri
pUri = do
  uriScheme <- dbg "scheme" pScheme
  void (char ':')
  uriAuthority <- dbg "auth" . optional . try $ do
    void (string "//")
    authUser <- dbg "user" . optional . try $ do
      user <- T.pack <$> some alphaNumChar
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> dbg "host" (some (alphaNumChar <|> char '.'))
    authPort <- dbg "port" $ optional (char ':' *> L.decimal)
    return Authority {..}
  return Uri {..}
```

Then let's try running `pUri` on that unfortunate input again:

```
λ> parseTest' (pUri <* eof) "https://mark:@example.com"
scheme> IN: "https://mark:@example.com"
scheme> MATCH (COK): "https"
scheme> VALUE: SchemeHttps

user> IN: "mark:@example.com"
user> MATCH (EOK): <EMPTY>
user> VALUE: Nothing

host> IN: "mark:@example.com"
host> MATCH (COK): "mark"
host> VALUE: "mark"

port> IN: ":@example.com"
port> MATCH (CERR): ':'
port> ERROR:
port> 1:14:
port> unexpected '@'
port> expecting integer

auth> IN: "//mark:@example.com"
auth> MATCH (EOK): <EMPTY>
auth> VALUE: Nothing

1:7:
  |
1 | https://mark:@example.com
  |       ^
unexpected '/'
expecting end of input
```

We can see what exactly is going on inside Megaparsec now:

* `scheme` matches successfully.

* `user` fails: although there is a username in place `mark`, there is no
  password after the column `:`. We fail and thanks to `try`, backtrack.

* `host` start from the same point as `user` and tries now to interpret
  input as hostname. We can see that it succeeds and returns `mark` as host
  name.

* There may be a port number after host, so `port` gets its chance now. It
  sees `:`, but after that there is no integer, so `port` fails as well.

* The whole `auth` parser thus fails (`port` is inside of `auth` and it has
  failed).

* The `auth` parser returns `Nothing` because it could not parse anything.
  Now `eof` demands that we have reached the end of input, but it's not the
  case, so we get the final error message.

What to do? This is an example of a situation when using `try` enclosing
large portions of code may make parse errors worse. Let's take another look
at the syntax we want to parse:

```
scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
```

What are we looking for? Something that would allow us to commit to certain
branch of parsing. Just like with port where when we see column `:` we're
sure port number must follow. If you look carefully, you'll see that the
double slash `//` is a certain sign that we have the authority part in our
URI. Since we match `//` with an “atomic” parser (`string`), matching on it
backtracks automatically, and after we have matched `//`, we can be fearless
and demand the authority part. Let's remove the first `try` from `pUri`:

```haskell
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme
  void (char ':')
  uriAuthority <- optional $ do -- removed 'try' on this line
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar
      void (char ':')
      password <- T.pack <$> some alphaNumChar
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.')
    authPort <- optional (char ':' *> L.decimal)
    return Authority {..}
  return Uri {..}
```

Now we get a nicer parse error:

```haskell
λ> parseTest' (pUri <* eof) "https://mark:@example.com"
1:14:
  |
1 | https://mark:@example.com
  |              ^
unexpected '@'
expecting integer
```

Although it's still a bit misleading, but well, that's a tricky example I've
picked. Lots of `optional`s.

## Labelling and hiding things

Sometimes the list of expected items may get rather long. Remember what we
get when we try to use a non-recognized scheme?

```haskell
λ> parseTest' (pUri <* eof) "foo://example.com"
1:1:
  |
1 | foo://example.com
  | ^
unexpected "foo://"
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

Megaparsec provides a way to override expected items with something custom,
typically called a *label*. This is done with help of (unsurprisingly) the
`label` primitive (which has a synonym in the form of the `(<?>)` operator):

```haskell
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  -- the rest stays the same
```

```haskell
λ> parseTest' (pUri <* eof) "foo://example.com"
1:1:
  |
1 | foo://example.com
  | ^
unexpected "foo://"
expecting valid scheme
```

We can go on and add more labels to make errors messages more
human-readable:

```haskell
pUri :: Parser Uri
pUri = do
  uriScheme <- pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- optional $ do
    void (string "//")
    authUser <- optional . try $ do
      user <- T.pack <$> some alphaNumChar <?> "username"
      void (char ':')
      password <- T.pack <$> some alphaNumChar <?> "password"
      void (char '@')
      return (user, password)
    authHost <- T.pack <$> some (alphaNumChar <|> char '.') <?> "hostname"
    authPort <- optional (char ':' *> label "port number" L.decimal)
    return Authority {..}
  return Uri {..}
```

For example:

```haskell
λ> parseTest' (pUri <* eof) "https://mark:@example.com"
1:14:
  |
1 | https://mark:@example.com
  |              ^
unexpected '@'
expecting port number
```

Another primitive is called `hidden`. If `label` renames things, `hidden`
just removes them altogether. Compare:

```haskell
λ> parseTest' (many (char 'a') >> many (char 'b') >> eof :: Parser ()) "d"
1:1:
  |
1 | d
  | ^
unexpected 'd'
expecting 'a', 'b', or end of input
λ> parseTest' (many (char 'a') >> hidden (many (char 'b')) >> eof :: Parser ()) "d"
1:1:
  |
1 | d
  | ^
unexpected 'd'
expecting 'a' or end of input
```

`hidden` is useful when you want to make your error messages less noisy. For
example, when parsing a programming language it's a good idea to drop
“expecting white space” messages because usually there may be white space
after each token anyway.

Finishing the `pUri` parser is left as an exercise for the reader, now that
all the tools that are necessary for this have been explained.

## Lexing

*Lexing* is the process of transforming input stream into a stream of
tokens: integers, keywords, symbols, etc. which are easier to parse than raw
input directly. Lexing can be performed in a separate pass with an external
tool such as [`alex`](https://hackage.haskell.org/package/alex), but
Megaparsec also provides functions that should simplify writing a lexer in a
seamless fashion, as part of your parser.

There are two lexer modules
[`Text.Megaparsec.Char.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html)
for character streams and
[`Text.Megaparsec.Byte.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte-Lexer.html)
for byte streams. We'll be using
[`Text.Megaparsec.Char.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html)
because we work with strict `Text` as input stream, but most functions are
mirrored in
[`Text.Megaparsec.Byte.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Byte-Lexer.html)
as well if you wish to work with `ByteString`s.

### White space

The first topic we need to cover is dealing with white space. It's helpful
to consume white space in a consistent manner either before every token or
after every token. Megaparsec's lexer modules follow the strategy “assume no
white space before token and consume all white space after token”.

To consume white space we need a special parser that we'll refer to as
*space consumer*. The
[`Text.Megaparsec.Char.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html)
provides a helper allowing to build a general space consumer:

```haskell
space :: MonadParsec e s m
  => m () -- ^ A parser for space characters which does not accept empty
          -- input (e.g. 'space1')
  -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
  -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
  -> m ()
```

The documentation for the `space` function is quite comprehensive by itself,
but let's complement it with an example:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L -- (1)

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)
```

Some notes:

1. The
   [`Text.Megaparsec.Char.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html)
   is intended to be imported qualified because it contains names that
   collide with names from e.g.
   [`Text.Megaparsec.Char`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html),
   for example `space`.

2. The first argument of `L.space` should be a parser that is to be used to
   pick up white space. An important detail is that it should not accept
   empty input because then `L.space` would go into an infinite loop.
   `space1` is a parser from
   [`Text.Megaparsec.Char`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html)
   that satisfies the requirements perfectly.

3. The second argument of `L.space` defines how to skip line comments, that
   is, comments that start with a given sequence of tokens and end with end
   of line. The `skipLineComment` helper allows to craft an auxiliary parser
   for line comments easily.

4. The third argument of `L.space` in turn defines how to pick up block
   comments: everything between starting and ending sequences of tokens. The
   `skipBlockComment` helper allows to deal with non-nested block comments.
   If supporting nested block comments is desirable,
   `skipBlockCommentNested` should be used instead.

Operationally, `L.space` tries all three parsers in turn as many times as it
can till all of them cannot be applied anymore meaning that we have consumed
all white space there is. Knowing this, it should make sense that if your
grammar does not include block or line comments, you can just pass `empty`
as the second/third argument of `L.space`. `empty`, being the identity of
`Alternative`s, will just cause `L.space` to try parser for next white space
component—exactly what is desirable.

Having the space consumer `sc`, we can then define various white
space-related helpers:

```haskell
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc -- (1)

symbol :: Text -> Parser Text
symbol = L.symbol sc -- (2)
```

1. `lexeme` is a wrapper for lexemes that picks up all trailing white space
   using the supplied space consumer.

2. `symbol` is a parser that matches given text using `string` internally
   and then similarly picks up all trailing white space.

We'll see how it all works together in a moment, but first we need to
introduce a couple more helpers from
[`Text.Megaparsec.Char.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html).

### Char and string literals

Parsing character and string literals can be tricky because of various
escaping rules. To make life easier, Megaparsec provides the `charLiteral`
parser:

```haskell
charLiteral :: (MonadParsec e s m, Token s ~ Char) => m Char
```

The job of `charLiteral` is to parse a single character that may be escaped
according to the syntax for character literals described in the Haskell
report. Note it does not parse quotes around the literal though for two
reasons:

* so the user can control how character literals are quoted,

* so `charLiteral` can be used to parse string literals as well.

Here are some example parsers built on top of `charLiteral`:

```haskell
charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral -- (1)

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"') -- (2)
```

1. To turn `L.charLiteral` into a parser for char literals we only need to
   add the enclosing quotes. Here we follow Haskell syntax and use single
   quotes. The `between` combinator is defined simply as: `between open
   close p = open *> p <* close`.

2. `stringLiteral` uses `L.charLiteral` to parse individual characters
   inside string literal enclosed in double quotes.

The second function is also interesting because of its use of the `manyTill`
combinator:

```haskell
manyTill :: Alternative m => m a -> m end -> m [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> ((:) <$> p <*> go)
```

It tries to apply the `end` parser on every iteration and if it fails, it
then runs the `p` parser and accumulates results of `p` in a list.

There is also `someTill` for when you want to demand that at least one item
is present.

### Numbers

Finally, a very common need is to parse numbers. For integral numbers, there
are three helpers that can parse values in decimal, octal, and hexadecimal
representations:

```haskell
decimal, octal, hexadecimal
  :: (MonadParsec e s m, Token s ~ Char, Integral a) => m a
```

Using them is easy:

```haskell
integer :: Parser Integer
integer = lexeme L.decimal
```

```haskell
λ> parseTest' (integer <* eof) "123  "
123
λ> parseTest' (integer <* eof) "12a  "
1:3:
  |
1 | 12a
  |   ^
unexpected 'a'
expecting end of input or the rest of integer
```

`scientific` and `float` accept integer and fractional grammars.
`scientific` returns the `Scientific` type from the
[`scientific`](https://hackage.haskell.org/package/scientific) package,
while `float` is polymorphic in its result type and can return any instance
of `RealFloat`:

```haskell
scientific :: (MonadParsec e s m, Token s ~ Char)              => m Scientific
float      :: (MonadParsec e s m, Token s ~ Char, RealFloat a) => m a
```

For example:

```haskell
float :: Parser Double
float = lexeme L.float
```

```haskell
λ> parseTest' (float <* eof) "123"
123.0
λ> parseTest' (float <* eof) "123.45"
123.45
λ> parseTest' (float <* eof) "123d"
1:4:
  |
1 | 123d
  |    ^
unexpected 'd'
expecting '.', 'E', 'e', end of input, or the rest of floating point number
```

Note that all these parsers do not parse signed numbers. To make a parser
for signed numbers, we need to wrap an existing parser with the `signed`
combinator:

```haskell
signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float
```

The first argument of `signed`—the space consumer—controls how white space
is consumed between the sign and actual numeral. If you don't want to allow
space in there, just pass `return ()` instead.

## `notFollowedBy` and `lookAhead`

There are two more primitives (in addition to `try`) that can perform look
ahead in input stream without actually advancing parsing position in it.

The first one is called `notFollowedBy`:

```haskell
notFollowedBy :: MonadParsec e s m => m a -> m ()
```

It succeeds only when its argument parser fails and never consumes any input
or modifies parser state.

As an example when you may want to use `notFollowedBy`, consider parsing of
keywords:

```haskell
pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword)
```

This parser has a problem: what if the keyword we're matching against is
just a prefix of an identifier? In that case it's definitely not a keyword.
Thus we must eliminate that case by using `notFollowedBy`:

```haskell
pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)
```

Another primitive is `lookAhead`:

```haskell
lookAhead :: MonadParsec e s m => m a -> m a
```

If the argument `p` of `lookAhead` succeeds, the whole construct `lookAhead
p` also succeeds but the input stream (and the entire parser state) stays
untouched, i.e. nothing is consumed.

One example of where this may be useful is performing a check on already
parsed input and then either failing or continuing successfully. The idiom
can be expressed in code like this:

```haskell
withPredicate1
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate1 f msg p = do
  r <- lookAhead p
  if f r
    then p
    else fail msg
```

This demonstrates a use of `lookAhead`, but we also should note that when
the check if successful we perform the parsing twice, which is not good.
Here is an alternative solution using the `getNextTokenPosition` function:

```haskell
withPredicate2
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate2 f msg p = do
  mpos <- getNextTokenPosition
  r    <- p
  if f r
    then return r
    else do
      forM_ mpos setPosition
      fail msg
```

**NOTE**: `getNextTokenPosition` is better than `getPosition` in this case
because it returns the position at which next token in the input stream
begins, not current position of parser. The difference is apparent when we
work with a stream of custom tokens where every token may have its starting
and ending positions attached to it.

## Parsing expressions

By “expression” we mean a structure formed from terms and operators applied
to those terms. Operators can be prefix, infix, and postfix, left and
right-associative, with different precedence. An example of such a construct
would be arithmetic expressions familiar from school:

```
a * (b + 2)
```

Here we can see two kinds of terms: variables (`a` and `b`) and integers
(`2`). There are also two operators: `*` and `+`.

Writing an expression parser may take a while to get right. To help with
that, Megaparsec comes with the
[`Text.Megaparsec.Expr`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Expr.html)
module which exports only two things: the `Operator` data type and the
`makeExprParser` helper. Both are well documented, so in this section we
won't repeat the documentation, instead we are going to write a simple but
fully functional expression parser.

Let's start by defining a data type representing expression as
[AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree):

```haskell
data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)
```

To use `makeExprParser` we need to provide it with term parser and “operator
table”:

```haskell
makeExprParser :: MonadParsec e s m
  => m a               -- ^ Term parser
  -> [[Operator m a]]  -- ^ Operator table, see 'Operator'
  -> m a               -- ^ Resulting expression parser
```

Let's start with term parser then. It's helpful to think about term as a box
that that is to be considered as a indivisible whole by the expression
parsing algorithm when it works with things like associativity and
precedence. In our case there are three things that fall into this category:
variables, integers, and entire expressions in parentheses. Using the
definitions from previous chapters we can define the term parser as:

```haskell
pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
  , pVariable
  , pInteger ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable = undefined -- TODO
```

The definitions of `pVariable`, `pInteger`, and `parens` should go without
questions by now. We're also quite lucky here in that we don't need `try`s
in `pTerm` because the grammars do not overlap:

* if we see an opening parenthesis `(`, we know that an expression in
  parentheses is to follow, so we commit to that branch;

* if we see a letter, we know that it's start of an identifier;

* if we see a digit, we know that it's start of an integer.

Finally, to finish `pExpr` we need to define the `operatorTable`. We can see
from the type that it's a nested list. Every inner list is a list of
operators we want to support, they all have equal precedence. The outer list
is ordered in descending precedence, so higher we place a group of operators
in it, tighter they bind:

```haskell
data Operator m a -- N.B.
  = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
  | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
  | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
  | Prefix  (m (a -> a))      -- ^ Prefix
  | Postfix (m (a -> a))      -- ^ Postfix

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id ]
  , [ binary "*" Product
    , binary "/" Division ]
  , [ binary "+" Sum
    , binary "-" Subtr ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
```

Note how we place `Parser (Expr -> Expr -> Expr)` inside `InfixL` in
`binary` and similarly `Parser (Expr -> Expr)` in `prefix` and `postfix`.
I.e. we run `symbol name` and return function to apply to terms in order to
get the final result of type `Expr`.

We can now try our parser, it's ready!

```haskell
λ> parseTest' (pExpr <* eof) "a * (b + 2)"
Product (Var "a") (Sum (Var "b") (Int 2))
λ> parseTest' (pExpr <* eof) "a * b + 2"
Sum (Product (Var "a") (Var "b")) (Int 2)
λ> parseTest' (pExpr <* eof) "a * b / 2"
Division (Product (Var "a") (Var "b")) (Int 2)
λ> parseTest' (pExpr <* eof) "a * (b $ 2)"
1:8:
  |
1 | a * (b $ 2)
  |        ^
unexpected '$'
expecting ')' or operator
```

Documentation for the
[`Text.Megaparsec.Expr`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Expr.html)
module contains some hints that are useful in certain less-standard
situations, so it's a good idea to read it as well.

## Indentation-sensitive parsing

HEREHERE

The [module](megaparsec:Text.Megaparsec.Char.Lexer) module contains tools
that should be helpful when parsing indentation-sensitive grammars. We are
going to review the available combinators first, then put them into use by
writing an indentation-sensitive parser.

### `nonIndented` and `indentBlock`

Let's start with the simplest thing -- `nonIndented`:

```
nonIndented :: MonadParsec e s m
  => m ()              -- ^ How to consume indentation (white space)
  -> m a               -- ^ Inner parser
  -> m a
```

It allows to make sure that its inner parser consumes input that is
[emph](not) indented. It's a part of a model behind high-level parsing of
indentation-sensitive input. We state that there are top-level items that
are not indented and that all indented tokens are directly or indirectly
<<children>> of those top-level definitions. In Megaparsec, we don't need
any additional state to express this. Since indentation is always relative,
our idea is to explicitly tie parsers for <<reference>> tokens and indented
tokens, thus defining indentation-sensitive grammar via pure combination of
parsers.

So, how do we define a parser for indented block? Let's take a look at the
signature of `indentBlock`:

```
indentBlock :: (MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume indentation (white space)
  -> m (IndentOpt m a b) -- ^ How to parse “reference” token
  -> m a
```

First, we specify how to consume indentation. An important thing to note
here is that this space-consuming parser [emph](must) consume newlines as
well, while tokens (reference token and indented tokens) should not normally
consume newlines after them.

As you can see, the second argument allows us to parse reference token and
return a data structure that tells `indentBlock` what to do next. There are
several options:

```
data IndentOpt m a b
  = IndentNone a
    -- ^ Parse no indented tokens, just return the value
  | IndentMany (Maybe Pos) ([b] -> m a) (m b)
    -- ^ Parse many indented tokens (possibly zero), use given indentation
    -- level (if 'Nothing', use level of the first indented token); the
    -- second argument tells how to get final result, and third argument
    -- describes how to parse an indented token
  | IndentSome (Maybe Pos) ([b] -> m a) (m b)
    -- ^ Just like 'IndentMany', but requires at least one indented token to
    -- be present
```

We can change our mind and parse no indented tokens, we can parse
[emph](many) (that is, possibly zero) indented tokens or require [emph](at
least one) such token. We can either allow `indentBlock` detect indentation
level of the first indented token and use that, or manually specify
indentation level.

### Parsing a simple indented list

Let's parse a simple indented list of some items. Let's begin with the
import section:

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Main (main) where

import Control.Applicative
import Control.Monad (void)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text
```

We will need two kinds of space-consumers: one that consumes new lines `scn`
and one that doesn't `sc` (actually it only parses spaces and tabs here):

```
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
```

Just for fun, we allow line comments that start with `#`.

`pItemList` is a top-level form that itself is a combination of reference
token (header of list) and indented tokens (list items), so:

```
pItemList :: Parser (String, [String]) -- header and list items
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)
```

For our purposes, an item is a sequence of alpha-numeric characters and
dashes:

```
pItem :: Parser String
pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"
```

Let's load the code into GHCi and try it with help of `parseTest'` built-in:

```
λ> parseTest' (pItemList <* eof) ""
1:1:
  |
1 | <empty line>
  | ^
unexpected end of input
expecting list item
λ> parseTest' (pItemList <* eof) "something"
("something",[])
λ> parseTest' (pItemList <* eof) "  something"
1:3:
  |
1 |   something
  |   ^
incorrect indentation (got 3, should be equal to 1)
λ> parseTest' (pItemList <* eof) "something\none\ntwo\nthree"
2:1:
  |
2 | one
  | ^
unexpected 'o'
expecting end of input
```

Remember that we're using the `IndentMany` option, so empty lists are OK, on
the other hand the built-in combinator `space` has hidden the phrase
<<expecting more space>> from error messages, so this error message is
perfectly reasonable.

Let's continue:

```
λ> parseTest' (pItemList <* eof) "something\n  one\n    two\n  three"
3:5:
  |
3 |     two
  |     ^
incorrect indentation (got 5, should be equal to 3)
λ> parseTest' (pItemList <* eof) "something\n  one\n  two\n three"
4:2:
  |
4 |  three
  |  ^
incorrect indentation (got 2, should be equal to 3)
λ> parseTest' (pItemList <* eof) "something\n  one\n  two\n  three"
("something",["one","two","three"])
```

This definitely seems to work. Let's replace `IndentMany` with `IndentSome`
and `Nothing` with `Just (mkPos 5)` (indentation levels are counted from 1,
so it will require 4 spaces before indented items):

```
pItemList :: Parser (String, [String])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome (Just (mkPos 5)) (return . (header, )) pItem)
```

Now:

```
λ> parseTest' (pItemList <* eof) "something\n"
2:1:
  |
2 | <empty line>
  | ^
incorrect indentation (got 1, should be greater than 1)
λ> parseTest' (pItemList <* eof) "something\n  one"
2:3:
  |
2 |   one
  |   ^
incorrect indentation (got 3, should be equal to 5)
λ> parseTest' (pItemList <* eof) "something\n    one"
("something",["one"])
```

First message may be a bit surprising, but Megaparsec knows that there must
be at least one item in the list, so it checks indentation level and it's 1,
which is incorrect, so it reports it.

### Nested indented list

Let's allow list items to have sub-items. For this we will need a new
parser, `pComplexItem`:

```
pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)

pItemList :: Parser (String, [(String, [String])])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome Nothing (return . (header, )) pComplexItem)
```

If I feed something like this:

```
first-chapter
  paragraph-one
      note-A # an important note here!
      note-B
  paragraph-two
    note-1
    note-2
  paragraph-three
```

…into our parser, I get:

```
Right
  ( "first-chapter"
  , [ ("paragraph-one",   ["note-A","note-B"])
    , ("paragraph-two",   ["note-1","note-2"])
    , ("paragraph-three", []) ] )
```

This demonstrates how this approach scales for nested indented construts
without requiring additional state.

### Adding line folds

A [def](line fold) consists of several elements that can be put on one line
or on several lines as long as indentation level of subsequent items is
greater than indentation level of the first item.

Let's make use of another helper called `lineFold`:

```
pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pLineFold)

pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
  let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
  in unwords <$> ps <* sc -- (1)
```

`lineFold` works like this: we give it a space consumer that accepts
newlines `scn` and it gives us a special space consumer `sc'` that we can
use in the callback to consume space between elements of line fold. An
important thing here is that we should use normal space consumer at the end
of line fold (1) or our fold will have no end (this is also the reason we
have to use `try` with `sc'`).

Playing with the final version of our parser is left as an exercise for the
reader. You can create <<items>> that consist of multiple words and as long
as they are line-folded they will be parsed and concatenated with single
space between them.

## Writing efficient parsers

Let's now discuss what to attempt to improve performance of a Megaparsec
parser. It should be noted right away that one should always check if there
is any improvement through profiling and benchmarking. That's the only way
to understand if we are doing the right thing when tuning performance.

Common pieces of advice:

* If your parser uses a monad stack instead of plain `Parsec` monad (recall
  that it's the `ParsecT` monad transformer over `Identity`, which is much
  more lightweight), make sure you use at least version 0.5 of
  `transformers` library, and at least version 6.0 of `megaparsec`. Both
  libraries have critical performance improvements in these versions, so you
  can just get better performance for free.

* `Parsec` monad will be always faster then `ParsecT`-based monad
  transformers. Avoid using `StateT`, `WriterT`, and other monad
  transformers unless absolutely necessary. When you have a relatively
  simple monad stack, for example with `StateT` and nothing more,
  performance of Megaparsec parser will be on par with Parsec. The more you
  add to the stack, the slower it will be.

* Backtracking is an expensive operation. Avoid building long chains of
  alternatives where every alternative can go deep into input before
  failing.

* Do not keep your parsers polymorphic unless you really have a reason to do
  so. It's best to fix the types of parsers specifying concrete types, such
  as `type Parser = Parsec Void Text` for every top-level definition. This
  way GHC will be able to optimize better.

* Inline generously (when it makes sense, of course). You may not believe
  your eyes when you see how much of a difference inlining can do,
  especially for short functions. This is especially true for parsers that
  are defined in one module and used in another one, because `INLINE` and
  `INLINEABLE` pragmas make GHC dump functions definitions into an interface
  file and this facilitates specializing.

* Use the fast primitives such as `takeWhileP`, `takeWhile1P`, and `takeP`
  whenever you can.
  [link=https://markkarpov.com/post/megaparsec-more-speed-more-power.html#there-is-hope](This
  blog post) explains why they are so fast.

* Avoid `oneOf` and `noneOf` preferring `satisfy` and `notChar` whenever
  possible.

While most of the points above do not require additional comment, I think
it'd be beneficial to get into the habit of using the newer fast primitives:
`takeWhileP`, `takeWhile1P`, and `takeP`. The first two are especially
common as they allow to replace some `many` and `some`-based constructs
making them faster and changing the type of returned data to chunk of input
stream, i.e. that `Tokens s` thing we have discussed previously.

For example, recall that when we parsed URIs, we had this code for parsing
username in the authority component:

```
  user <- T.pack <$> some alphaNumChar
```

We can replace it by `takeWhile1P`:

```
  user <- takeWhile1P (Just "alpha num character") isAlphaNum
  --                  ^                            ^
  --                  |                            |
  -- label for tokens we match against         predicate
```

When we parse `ByteString`s and `Text`, this will be a lot faster than the
original approach, also note that `T.pack` is not necessary anymore as we
get `Text` directly from `takeWhile1P`.

These equations should be helpful in understanding the `Maybe String`
argument of `takeWhileP` and `takeWhile1P`:

```
takeWhileP  (Just "foo") f = many (satisfy f <?> "foo")
takeWhileP  Nothing      f = many (satisfy f)
takeWhile1P (Just "foo") f = some (satisfy f <?> "foo")
takeWhile1P Nothing      f = some (satisfy f)
```

## Parse errors

Now that we have explored how to use most features of Megaparsec, it's time
to learn more about parse errors: how they are defined, how to signal them,
and how to process them inside a running parser.

The `ParseError` type is defined like this:

```
data ParseError t e
  = TrivialError (NonEmpty SourcePos) (Maybe (ErrorItem t)) (Set (ErrorItem t))
    -- ^ Trivial errors, generated by Megaparsec's machinery. The data
    -- constructor includes the stack of source positions, unexpected token
    -- (if any), and expected tokens.
  | FancyError (NonEmpty SourcePos) (Set (ErrorFancy e))
    -- ^ Fancy, custom errors.
  deriving (Show, Read, Eq, Data, Typeable, Generic)
```

In English: a `ParseError` is either a `TirivalError` with at most one
unexpected item and a (possibly empty) collection of expected items or a
`FancyError`.

`ParseError t e` is parametrized over two type variables:

* `t` is the type of token, i.e. `Token s` if we parse stream `s`.
* `e` is the type of custom component of parse error.

`ErrorItem` is defined as:

```
data ErrorItem t
  = Tokens (NonEmpty t)      -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)
```

And here is `FancyError`:

```
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

`FancyError` includes data constructors for two common cases Megaparsec
supports out-of-the-box:

* Use of the `fail` function that causes parser to fail reporting an
  arbitrary `String`.

* Indentation issues which we have discussed in a previous section. Since we
  provide tools for working with indentation-sensitive grammars
  out-of-the-box, we need a way to store well-typed information about
  problems with indentation.

Finally, `ErrorCustom` is a sort of <<extension slot>> which allows to embed
arbitrary data into the `ErrorFancy` type. When we don't need any custom
data in our parse errors, we parametrize `ErrorFancy` by `Void`. Since
`Void` is not inhabited by non-bottom values, `ErrorCustom` becomes
<<cancelled>> or if we follow the analogy between algebraic data types and
numbers, <<multiplied by zero>>.

Let's discuss different ways to signal a parse error. The simplest function
for that is `fail`:

```
λ> parseTest' (fail "I'm failing, help me!" :: Parser ()) ""
1:1:
  |
1 | <empty line>
  | ^
I'm failing, help me!
```

For many people who are familiar with simpler parsing libraries such as
Parsec this is often enough. However, displaying a parse error to the user
is not everything, we may have a need to analyze and/or manipulate it. This
is where `String`s are certainly not very convenient.

Trivial parse errors are usually generated by Megaparsec, but we can signal
any such an error ourselves using the `failure` primitive:

```
failure :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ Unexpected item (if any)
  -> Set (ErrorItem (Token s)) -- ^ Expected items
  -> m a
```

```
unfortunateParser :: Parser ()
unfortunateParser = failure (Just EndOfInput) (Set.fromList es)
  where
    es = [Tokens (NE.fromList "a"), Tokens (NE.fromList "b")]
```

```
λ> parseTest' unfortunateParser ""
1:1:
  |
1 | <empty line>
  | ^
unexpected end of input
expecting 'a' or 'b'
```

Unlike `fail`-based approach, trivial parse errors are easy to pattern-match
on, inspect, and modify.

For fancy errors we correspondingly have the `fancyFaliure` primitive:

```
fancyFailure :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m a
```

With `fancyFailure`, it's often desirable to define a helper like the one we
have in the lexer modules instead of calling `fancyFailure` directly:

```
incorrectIndent :: MonadParsec e s m
  => Ordering  -- ^ Desired ordering between reference level and actual level
  -> Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Actual indentation level
  -> m a
incorrectIndent ord ref actual = fancyFailure . E.singleton $
  ErrorIndentation ord ref actual
```

As an example of adding a custom parse error component to your parser, let's
go through defining a special parse error that says that given `Text` value
is not a keyword.

First we need to define the data type with constructors representing
scenarios we want to support:

```
data Custom = NotKeyword Text
  deriving (Eq, Show, Ord)
```

And tell Megaparsec how to display it in parse errors:

```
instance ShowErrorComponent Custom where
  showErrorComponent (NotKeyword txt) = T.unpack txt ++ " is not a keyword"
```

Next we update our `Parser` type synonym:

```
type Parser = Parsec Custom Text
```

After that we can define the `notKeyword` helper:

```
notKeyword :: Text -> Parser a
notKeyword = fancyFailure . Set.singleton . ErrorCustom . NotKeyword
```

Finally, let's try it:

```
λ> parseTest' (notKeyword "foo" :: Parser ()) ""
1:1:
  |
1 | <empty line>
  | ^
foo is not a keyword
```

Speaking of displaying parse errors, there is `parseErrorPretty` family of
functions for that:

* `parseErrorPretty` prints parse errors without including the relevant line
  into the print-out.

* `parseErrorPretty'` is the same, but includes the relevant line.

* `parseErrorPretty_` is just like `parseErrorPretty'`, but allows to set
  non-default tab width.

Playing with these functions is left as an exercise for the reader.

Another useful feature of Megaparsec is that it's possible to <<catch>> a
parse error, alter it in some way and then re-throw, just like with
exceptions. This is enabled by the `observing` primitive:

```
-- | @'observing' p@ allows to “observe” failure of the @p@ parser, should
-- it happen, without actually ending parsing, but instead getting the
-- 'ParseError' in 'Left'. On success parsed value is returned in 'Right'
-- as usual. Note that this primitive just allows you to observe parse
-- errors as they happen, it does not backtrack or change how the @p@
-- parser works in any way.

observing :: MonadParsec e s m
  => m a             -- ^ The parser to run
  -> m (Either (ParseError (Token s) e) a)
```

Here is a complete program demonstrating typical usage of `observing`:

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main (main) where

import Control.Applicative
import Data.List (intercalate)
import Data.Set (Set)
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as Set

data Custom
  = TrivialWithLocation
    [String] -- position stack
    (Maybe (ErrorItem Char))
    (Set (ErrorItem Char))
  | FancyWithLocation
    [String] -- position stack
    (ErrorFancy Void) -- Void, because we don't want allow to nest Customs
  deriving (Eq, Ord, Show)

instance ShowErrorComponent Custom where
  showErrorComponent (TrivialWithLocation stack us es) =
    parseErrorTextPretty (TrivialError @Char @Void undefined us es)
      ++ showPosStack stack
  showErrorComponent (FancyWithLocation stack cs) =
    showErrorComponent cs
      ++ showPosStack stack

showPosStack :: [String] -> String
showPosStack = intercalate ", " . fmap ("in " ++)

type Parser = Parsec Custom Text

inside :: String -> Parser a -> Parser a
inside location p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) ->
      fancyFailure . Set.singleton . ErrorCustom $
        TrivialWithLocation [location] us es
    Left (FancyError _ xs) -> do
      let f (ErrorFail msg) = ErrorCustom $
            FancyWithLocation [location] (ErrorFail msg)
          f (ErrorIndentation ord rlvl alvl) = ErrorCustom $
            FancyWithLocation [location] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (TrivialWithLocation ps us es)) = ErrorCustom $
            TrivialWithLocation (location:ps) us es
          f (ErrorCustom (FancyWithLocation ps cs)) = ErrorCustom $
            FancyWithLocation (location:ps) cs
      fancyFailure (Set.map f xs)
    Right x -> return x

myParser :: Parser String
myParser = some (char 'a') *> some (char 'b')

main :: IO ()
main = do
  parseTest' (inside "foo" myParser) "aaacc"
  parseTest' (inside "foo" $ inside "bar" myParser) "aaacc"
```

If I run this program, I see the following output:

```
1:4:
  |
1 | aaacc
  |    ^
unexpected 'c'
expecting 'a' or 'b'
in foo
1:4:
  |
1 | aaacc
  |    ^
unexpected 'c'
expecting 'a' or 'b'
in foo, in bar
```

Thus, the feature can be used to attach location labels to parse errors, or
define [def](regions) in which parse errors are processed in some way. The
idiom is quite useful, so there is even a non-primitive helper called
`region` built in terms of the `observing` primitive:

```
-- | Specify how to process 'ParseError's that happen inside of this
-- wrapper. As a side effect of the current implementation changing
-- 'errorPos' with this combinator will also change the final 'statePos' in
-- the parser state (try to avoid that because 'statePos' will go out of
-- sync with factual position in the input stream, which is probably OK if
-- you finish parsing right after that, but be warned).

region :: MonadParsec e s m
  => (ParseError (Token s) e -> ParseError (Token s) e)
     -- ^ How to process 'ParseError's
  -> m a
     -- ^ The “region” that the processing applies to
  -> m a
region f m = do
  r <- observing m
  case r of
    Left err ->
      case f err of
        TrivialError pos us ps -> do
          updateParserState $ \st -> st { statePos = pos }
          failure us ps
        FancyError pos xs -> do
          updateParserState $ \st -> st { statePos = pos }
          fancyFailure xs
    Right x -> return x
```

As an exercise, rewrite the `inside` function in the program above using
`region`.

## Testing Megaparsec parsers

Testing a parser is a practical task most people face sooner or later, so we
are bound to cover it. The recommended way to test Megaparsec parsers (at
the time of this writing) is using the [package](hspec-megaparsec) package.
The package adds utility expectations such as `shouldParse`,
`parseSatisfies`, etc. which work with the [package](hspec) testing
framework.

Let's start with an example:

```
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Data.Text (Text)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

myParser :: Parser String
myParser = some (char 'a')

main :: IO ()
main = hspec $
  describe "myParser" $ do
    it "returns correct result" $
      parse myParser "" "aaa" `shouldParse` "aaa"
    it "result of parsing satisfies what it should" $
      parse myParser "" "aaaa" `parseSatisfies` ((== 4) . length)
```

`shouldParse` accepts `Either (ParseError t e) a` -- result of parsing and a
thing of the type `a` to compare with. It's probably the most common helper.
`parseSatisfies` is quite similar, but instead of comparing for equality
with expected result, it allows to check the result by applying an arbitrary
predicate.

Other simple expectations are `shouldSucceedOn` and `shouldFailOn` (although
they are rather rarely used):

```
    it "should parse 'a's all right" $
      parse myParser "" `shouldSucceedOn` "aaaa"
    it "should fail on 'b's" $
      parse myParser "" `shouldFailOn` "bbb"
```

With Megaparsec we want to be precise about parse errors our parsers
produce. To test parse errors there is `shouldFailWith`, which can be used
like this:

```
    it "fails on 'b's producing correct error message" $
      parse myParser "" "bbb" `shouldFailWith`
        TrivialError
          (initialPos "" :| [])
          (Just (Tokens ('b' :| [])))
          (Set.singleton (Tokens ('a' :| [])))
```

Writing out a `TrivialError` like this is tiresome. The definition of
`ParseError` contains <<inconvenient>> types like `Set` and `NonEmpty` which
are not handy to enter directly as we have just seen. Fortunately,
[module](hspec-megaparsec:Test.Hspec.Megaparsec) also re-exports the
[module](megaparsec:Text.Megaparsec.Error.Builder) module which provides an
API for easier construction of `ParserError`s. Let's instead use the `err`
helper:

```
    it "fails on 'b's producing correct error message" $
      parse myParser "" "bbb" `shouldFailWith` err posI (utok 'b' <> etok 'a')
```

* The first argument of `err` controls position of parse error. `posI`
  stands for <<initial position>>. This is what we want this time. Another
  option is to use `posN :: Stream s => Int -> s -> NonEmpty SourcePos`. The
  first argument is the number of tokens to consume `n` and the second
  argument is the input stream to consume `s`. Returned value is the
  position we arrive at after consuming `n` tokens from `s`.

* `utok` stands for <<unexpected token>>, similarly `etok` means <<expected
  token>>.

To construct fancy parse errors there is `errFancy` which should be easy to
figure out, so we won't comment on it here.

Finally, it's possible to test which part of input remains unconsumed after
parsing using `failsLeaving` and `succeedsLeaving`:

```
    it "consumes all 'a's but doesn't touch 'b's" $
      runParser' myParser (initialState "aaabbb") `succeedsLeaving` "bbb"
    it "fails without consuming anything" $
      runParser' myParser (initialState "bbbccc") `failsLeaving` "bbbccc"
```

These should by used with `runParser'` or `runParserT'` which accept custom
initial state of parser and return its final state (this is what allows to
check leftover of input stream after parsing):

```
runParser'
  :: Parsec e s a      -- ^ Parser to run
  -> State s           -- ^ Initial state
  -> (State s, Either (ParseError (Token s) e) a)

runParserT' :: Monad m
  => ParsecT e s m a   -- ^ Parser to run
  -> State s           -- ^ Initial state
  -> m (State s, Either (ParseError (Token s) e) a)
```

The `initialState` takes input stream and returns initial state with that
input stream and other record fields filled with default values.

Other sources of inspiration for using [package](hspec-megaparsec) are:

* [link=https://github.com/mrkkrp/megaparsec/tree/master/tests](Megaparsec's
  own test suite) written using [package](hspec-megaparsec).

* The
  [link=https://github.com/mrkkrp/hspec-megaparsec/blob/master/tests/Main.hs](toy
  test suite) that comes with [package](hspec-megaparsec) itself.

## Working with custom input streams

Megaparsec can be used to parse any input that is an instance of the
`Stream` type class. This means that it may be used in conjunction with a
lexing tool such as [package](alex).

Not to digress from our main topic by presenting how a stream of tokens
could be generated with [package](alex), we'll assume it in the following
form:

```
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Void
import Text.Megaparsec
import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

data MyToken
  = Int Int
  | Plus
  | Mul
  | Div
  | OpenParen
  | CloseParen
  deriving (Eq, Ord, Show)
```

To report parse errors though we need a way to know where token starts and
ends, so let's add `WithPos`:

```
data WithPos a = WithPos
  { startPos :: SourcePos
  , endPos   :: SourcePos
  , tokenVal :: a
  } deriving (Eq, Ord, Show)
```

Then we can have a data type for our stream:

```
newtype MyStream = MyStream [WithPos MyToken]
```

Next, we need to make `MyStream` an instance of the `Stream` type class.
This requires the `TypeFamilies` language extension because we want to
define the associated type functions `Token` and `Tokens`:

```
instance Stream MyStream where
  type Token  MyStream = WithPos MyToken
  type Tokens MyStream = [WithPos MyToken]
  -- …
```

`Stream` is well documented in the
[module](megaparsec:Text.Megaparsec.Stream) module. Here we go straight to
defining the missing methods:

```
  -- …
  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  positionAt1 Proxy _ = startPos
  positionAtN Proxy opos [] = opos
  positionAtN Proxy _ (wpos:_) = startPos wpos
  advance1 Proxy _ _ = endPos
  advanceN Proxy _ opos [] = opos
  advanceN Proxy _ _ (wpos:_) = endPos wpos
  take1_ (MyStream []) = Nothing
  take1_ (MyStream (t:ts)) = Just (t, MyStream ts)
  takeN_ n (MyStream s)
    | n <= 0    = Just ([], MyStream s)
    | null s    = Nothing
    | otherwise =
        let (x, s') = splitAt n s
        in Just (x, MyStream s')
  takeWhile_ f (MyStream s) =
    let (x, s') = DL.span f s
    in (x, MyStream s')
```

More background information about the `Stream` type class (and why it looks
like this) can be found in
[link=https://markkarpov.com/post/megaparsec-more-speed-more-power.html](this
blog post).

Megaparsec also needs to know how to display tokens in parse errors. Thus we
need to define a couple of instances:

```
instance ShowToken a => ShowToken (WithPos a) where
  showTokens = DL.intercalate ", "
    . NE.toList
    . fmap (showTokens . f . tokenVal)

instance ShowToken MyToken where
  showTokens = DL.intercalate ", "
    . NE.toList
    . fmap f
    where
      f (Int n)    = show n
      f Plus       = "+"
      f Mul        = "*"
      f Div        = "/"
      f OpenParen  = "("
      f CloseParen = ")"

nes :: a -> NonEmpty a
nes a = a :| []
```

The `intercalate ", "` part is quite dummy as we'll mostly print single
tokens, we have it because the `showTokens :: NonEmpty (Token s) -> String`
method forces us to be able to handle more than one token.

Now we can define `Parser` for our custom stream:

```
type Parser = Parsec Void MyStream
```

The next step is to define basic parsers on top of `token` and (if it makes
sense) `tokens` primitives. For the streams that are supported
out-of-the-box we have [module](megaparsec:Text.Megaparsec.Byte) and
[module](megaparsec:Text.Megaparsec.Char) modules, but if we are to work
with custom tokens, we need custom helpers.

```
liftMyToken :: MyToken -> WithPos MyToken
liftMyToken myToken = WithPos pos pos myToken
  where
    pos = initialPos ""

pToken :: MyToken -> Parser MyToken
pToken c = token test (Just (liftMyToken c))
  where
    f = Tokens . nes
    test wpos@(WithPos _ _ x) =
      if x == c
        then Right x
        else Left (pure (f wpos), Set.singleton (f (liftMyToken c)))

pInt :: Parser Int
pInt = token test Nothing <?> "integer"
  where
    f = Tokens . nes
    test (WithPos _ _ (Int n)) = Right n
    test wpos = Left (pure (f wpos), Set.empty)
```

Finally let's have a test parser which parses a sum:

```
pSum :: Parser (Int, Int)
pSum = do
  a <- pInt
  _ <- pToken Plus
  b <- pInt
  return (a, b)
```

And example input for it:

```
exampleStream :: MyStream
exampleStream = MyStream
  [ at 1 1 (Int 5)
  , at 1 3 Plus         -- (1)
  , at 1 5 (Int 6) ]
  where
    at  l c = WithPos (at' l c) (at' l (c + 1))
    at' l c = SourcePos "" (mkPos l) (mkPos c)
```

Let's try it:

```
λ> parseTest (pSum <* eof) exampleStream
(5,6)
```

If we change `Plus` on the line (1) to `Div`, we'll get the correct parse
error:

```
λ> parseTest (pSum <* eof) exampleStream
1:3:
unexpected /
expecting +
```

In other words, we have now a fully functional parser that parses a custom
stream.

## Conclusion

TODO: Rewrite the conclusion.

This concludes our investigation of how to use Megaparsec for writing a
parser in Haskell. Megaparsec strikes a nice balance between speed,
flexibility, and good parse errors. While there are safer (and also
less-powerful) solutions like [package](Earley), faster solution in the form
of the [package](attoparsec) package, in most cases Megaparsec provides best
overall value.
