---
title: First steps with Megaparsec
desc: Start your adventure in parsing here!
difficulty: 1
date:
  published: May 1, 2017
---

Are you looking for a way to start with parsing in Haskell? Then you have
come to the right place. This tutorial will introduce you to Megaparsec — an
advanced parsing library written in Haskell. While Megaparsec is capable of
a lot, it's also easy to use and easy to start with. In this tutorial we
will learn by writing a parser
for [URIs](https://en.wikipedia.org/wiki/Uniform_Resource_Identifier).

## The URI syntax

Let's take a look at the URI syntax:

```
scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
```

We should remember that things in square brackets `[]` are optional, they
may or may not appear in a valid URI. `[]` may be even nested to express a
possibility inside another possibility. We will handle all of this.

## Parsing the scheme

Let's include some modules:

```haskell
module Main (main) where

import Text.Megaparsec
import Text.Megaparsec.String
```

We will start with `scheme`. We will accept only schemes that are known to
us, such as:

* `data`
* `file`
* `ftp`
* `http`
* `https`
* `irc`
* `mailto`

To match a fixed sequence of characters, `string` is the best tool. To
express a choice, we use the `(<|>)` operator:

```haskell
pScheme :: Parser String
pScheme = string "data" <|> string "file"
```

We could go on:

```haskell
pScheme :: Parser String
pScheme = string "data" <|> string "file" <|> string "ftp" <|> …
```

But it's not nice to be so repetitive. The `choice` combinator allows to
have as many alternatives as we want, and it looks nicer:

```haskell
pScheme :: Parser String
pScheme = choice
  [ string "data"
  , string "file"
  , string "ftp"
  , string "http"
  , string "https"
  , string "irc"
  , string "mailto" ]
```

We already have something to play with. How to run it? For now, `parseTest`
will suffice. Load the code we have so far into the GHCi and execute:

```
λ> parseTest pScheme ""
1:1:
unexpected end of input
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
λ> parseTest pScheme "dat"
1:1:
unexpected "dat" or 'd'
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
λ> parseTest pScheme "file"
"file"
λ> parseTest pScheme "irc"
"irc"
```

Note that we get nice error messages for free.

After scheme, there should be a colon `:`. To require something to go after
something else, we use monadic bind or `do`-notation:

```haskell
data Uri = Uri
  { uriScheme :: String
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  r <- pScheme
  char ':'
  return (Uri r)
```

If you don't understand what is going on with all these nasty monads, just
go on with the tutorial, it'll click eventually.

If we try to run `pUri`, we'll see that it requires `:` to follow the scheme
name now:

```
λ> parseTest pUri "irc"
1:4:
unexpected end of input
expecting ':'
λ> parseTest pUri "irc:"
Uri {uriScheme = "irc"}
```

We are not done with the scheme parsing though. A good Haskell programmer
tries to define types in such a way so incorrect data simply cannot be
represented. Not any `String` is a valid scheme. Let's define a data type to
represent schemes and make our `pScheme` parser return value of that type:

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

The little friend `(<$)` just replaces output that parser on its right hand
side is going to return:

```
λ> parseTest pUri "https:"
1:5:
unexpected 's'
expecting ':'
```

Not good! `https` should be a valid scheme. Can you figure out what goes
wrong? The parser tries the alternatives one by one, and `http` matches, so
it does not go further to try `https`. The solution is to put the
`SchemeHttps <$ string "https"` line before the `SchemeHttp <$ string
"http"` line. Remember: *with alternatives, order matters*!

Here we go then:

```
λ> parseTest pUri "https:"
Uri {uriScheme = SchemeHttps}
λ> parseTest pUri "http:"
Uri {uriScheme = SchemeHttp}
λ> parseTest pUri "mailto:"
Uri {uriScheme = SchemeMailto}
λ> parseTest pUri "foo:"
1:1:
unexpected "fo" or 'f'
expecting "data", "file", "ftp", "http", "https", "irc", or "mailto"
```

Why does it say “unexpected "fo" or 'f'”? Well, `fo` is the actual input up
to the first mismatching character (it's not good to show all the input, so
we stop on first mismatching character). On the other hand, `f` is the first
letter and since we have options that do not start with `f`, it's possible
that the very first character was wrong. This has to do with how Megaparsec
generates the error messages, it simply tries the alternatives and knows how
to combine individual error messages (and when to combine them). If we try
this:

```
λ> parseTest (string "data" :: Parser String) "foo"
1:1:
unexpected 'f'
expecting "data"
```

We can see where the `f` comes from.

## Parsing user, password, host, and port

## How to run Megaparsec parsers

## Alternatives

## `many` and `some`

## Conclusion

Megaparsec has not only tutorials, but a nice documentation as well. You
will find that looking
at [the Haddocks](https://hackage.haskell.org/package/megaparsec) is often
enough to find out how to do certain things.
