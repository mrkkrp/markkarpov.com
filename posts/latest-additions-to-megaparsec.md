---
title: Latest additions to Megaparsec
author: Mark Karpov
description: This blog post summarizes new features added to Megaparsec in the second half of 2016.
published: November 24, 2016
---

I think it's time for a little blog post summarizing progress of the
Megaparsec project in the second half of 2016. There are quite a few new
things I have never announced and I fear that from the changelog alone it
isn't obvious how useful they are.

## The `observing` primitive

This is a new method in the `MonadParsec` type class (added in 5.1.0), which
looks like this:

```haskell
observing :: MonadParsec e s m => m a -> m (Either (ParseError (Token s) e) a)
```

As you may have guessed from the signature alone, it allows to “observe”
parser failure without actually ending the parsing. It does not backtrack or
change behavior of `m a` parser in any way, but allows you to do something
after failure.

[This issue](https://github.com/mrkkrp/megaparsec/issues/111) shows why we
decided to add this. Thanks to [Mike Ledger](https://github.com/mikeplus64)
for opening it, as I myself didn't think about such use case. In short, Mike
wanted to annotate `ParseError`s and “label expected items by the parser
that they came from”. After some discussion we came to this new primitive
that is a bit more general than a labelling helper. The following program
demonstrates its main use case:

```haskell
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.List (intercalate)
import Text.Megaparsec
import qualified Data.Set as S

data Pec = Pec [String] (Maybe Dec) -- (1)
  deriving (Show, Read, Eq, Ord)

instance ErrorComponent Pec where -- (2)
  representFail =
    Pec [] . Just . representFail
  representIndentation ord p0 p1 =
    (Pec [] . Just) (representIndentation ord p0 p1)

instance ShowErrorComponent Pec where -- (3)
  showErrorComponent (Pec ls dec) =
    let formatLabels = intercalate ", " . fmap ("in " ++)
    in maybe "" ((++ "\n") . showErrorComponent) dec ++ formatLabels ls

type Parser = Parsec Pec String -- (4)

attachLabel :: String -> Parser a -> Parser a
attachLabel l m = do
  r <- observing m -- (5)
  case r of
    Left ParseError {..} ->
      failure errorUnexpected errorExpected $ -- (6)
        if S.null errorCustom
          then S.singleton (Pec [l] Nothing)
          else S.map f errorCustom
      where
        f (Pec ls x) = Pec (l:ls) x
    Right x -> return x

foo :: Parser Char
foo = attachLabel "foo" bar

bar :: Parser Char
bar = attachLabel "bar" (char 'a')

main :: IO ()
main = parseTest foo "b"
```

If the code is puzzling to you, start with [this tutorial](https://mrkkrp.github.io/megaparsec/tutorials/custom-error-messages.html) I
have written by popular request to explain how to do custom error messages
with Megaparsec 5. Here I'll just explain how the labelling feature works.

1. The `Pec` data type represents custom part of parse errors. By design it
   must be able to represent two things: message that was given to `fail`
   and typed data about indentation error. In each case we want to support a
   stack of labels, so we start with `[String]` and add `Dec` — default
   error component that comes with Megaparsec out-of-box. Finally, even if
   we don't have anything custom (that `Dec` represents), we want a place
   where to keep the stack anyway, so we need to allow `Dec` to be missing,
   hence the `Maybe` wrapper.

2. `representFail` and `representIndentation` are used by the library to
   encode information when `fail` is used or indentation error occures. We
   just reuse the `Dec` instance and wrap it with empty label stack.

3. Here we have a chance to decide how to display the error component. We
   just display `Dec` if it's there, but in any case we want to append the
   label stack.

4. This is the type of parser we will be using.

5. We use `observing` because in case of failure we don't want to end
   parsing immediately as we would have no chance to attach anything or
   otherwise influence the generated `ParseError`.

6. In case of failure we either create a new error component if the set is
   empty, or grow every element of the set consing a label to it. (Remember,
   this is a `Set` because there may be several branches of parsing, each
   failing at the same source position, so every component must be
   mergeable. Most of the time the `Set` will contain only one element
   though.)

This program prints:

```
1:1:
unexpected 'b'
expecting 'a'
in foo, in bar
```

This new sort of label (not to be confused with `label` and `(<?>)`) may be
constructed dynamically, allowing tracking where exactly parse error
happened.

Now we are fully equipped to write a Megaparsec JSON parser with nice error
messages :-D I believe it could report very precisely where a parse error
occurred. Unfortunately I myself don't have the time and much need for it,
but maybe someone reading this post could write such a parser? It would a
nice library to have.

## Improved support for testing

For quite some time there has been a library
called
[`hspec-megaparsec`](https://hackage.haskell.org/package/hspec-megaparsec),
which provides helpers for testing Megaparsec parsers with Hspec. I never
announced it and I'm not sure how many Megaparsec users know about it, but
this is now the recommended way to test Megaparsec parsers.

Just to get a taste of the library, testing looks like this:

```haskell
describe "eol" $ do
  context "when stream begins with a newline" $
    it "succeeds returning the newline" $
      property $ \s -> do
        let s' = '\n' : s
        prs  eol s' `shouldParse`     "\n"
        prs' eol s' `succeedsLeaving` s
  context "when stream begins with CRLF sequence" $
    it "parses the CRLF sequence" $
      property $ \s -> do
        let s' = '\r' : '\n' : s
        prs  eol s' `shouldParse`     "\r\n"
        prs' eol s' `succeedsLeaving` s

-- Helpers:
-- prs p = parse p ""
-- prs' p s = runParser' p (initialState s)
```

I think this is very nice. In version 5.1, I have rewritten the entire
Megaparsec's test suite using Hspec and `hspec-megaparsec` (previously it
was a combination of `HUnit`, `QuickCheck`, and `test-framework` as a glue).
This forced one addition to `hspec-megaparsec` without which it could not be
used in Megaparsec's test suite: combinators for parse error construction.

One advantage of Megaparsec over Parsec is its clear, typed, extensible, and
comparable for equality parse errors. In Megaparsec test suite there are
literally hundreds of tests that check exactly which parse errors we get in
every case. On one hand `ParseError` record is really great as every
possible existing value of type `ParseError` is a valid parse error that
doesn't need any normalization. On the other hand, using `Set`s and
`NonEmpty` lists in `ParseError` makes it a bit verbose to work with.

The solution is now included in `hspec-megaparsec` in the form of a simple
set of monoidal values to build reference `ParseError`s from, take a look:

```haskell
context "when stream begins with '\\r', but it's not followed by '\\n'" $
  it "signals correct parse error" $
    property $ \ch -> ch /= '\n' ==> do
      let s = ['\r',ch]
      prs eol s `shouldFailWith` err posI
        (utoks s <> utok '\r' <> elabel "end of line")
context "when input stream is '\\r'" $
  it "signals correct parse error" $
    prs eol "\r" `shouldFailWith` err posI
      (utok '\r' <> elabel "end of line")
context "when stream does not begin with newline or CRLF sequence" $
  it "signals correct parse error" $
    property $ \ch s -> (ch `notElem` "\r\n") ==> do
      let s' = ch : s
      prs eol s' `shouldFailWith` err posI
        (utok ch <> elabel "end of line")
context "when stream is empty" $
  it "signals correct parse error" $
    prs eol "" `shouldFailWith` err posI
      (ueof <> elabel "end of line")
```

`err` takes position (`posI` means initial position) and a value that
describes components of a parse error. `utoks` stands for “unexpected
tokens”, `elabel` means “expecting thing with this label”, etc. See
[the documentation](https://hackage.haskell.org/package/hspec-megaparsec-0.3.0/docs/Test-Hspec-Megaparsec.html#g:3) for full list of helpers.

Another testing-related addition in version 5.1 is that most types in
Megaparsec now have `Arbitrary` instances, so you don't need to define
orphan instances again and again in your test suites.

## Improved support for debugging

Debugging a Megaparsec parser can be frustrating. Even if you understand
exactly how evaluation proceeds, mentally going through the parser is a lot
of work. Megaparsec 5.1 adds `dbg` — a debugging helper. It's very useful!

Suppose you have a parser that behaves strangely:

```haskell
stringLiteral :: Parser String
stringLiteral = catMaybes <$> (char '"' >> manyTill ch (char '"'))
  where ch = (Just <$> L.charLiteral) <|> (Nothing <$ string "\\&")
```

The parser `stringLiteral` should parse string literals respecting the `\&`
combination that can delimit characters as in `\x123\&4` (which is two
characters long, without `\&`, `4` would merge with the previous character
`\x123`). Let's run it:

```
λ> parseTest stringLiteral "\"\\x123\\&4\""
"\291\&4"
```

When I first got this result it wasn't obvious to me that `\&` is not there,
it's just how pretty-printer prints the string, it inserts `\&` to delimit
the characters. So I wondered for a moment where did `\&` come from (how
silly of me!). With `dbg` it's easy to understand what's going on:

```haskell
stringLiteral :: Parser String
stringLiteral = catMaybes <$>
  (dbg "open" (char '"') >> manyTill ch (dbg "close" $ char '"'))
  where ch = (Just    <$> dbg "lit" L.charLiteral)
         <|> (Nothing <$  dbg "del" (string "\\&"))
```

`dbg label p` takes a label `label` and a parser `p` and works just like
`p`, except it lets you know what is going on in Megaparsec's internals:

```
λ> parseTest stringLiteral "\"\\x123\\&4\""
open> IN: ""\x123\&4""
open> MATCH (COK): '"'
open> VALUE: '"'

close> IN: "\x123\&4""
close> MATCH (EERR): <EMPTY>
close> ERROR:
close> 1:2:
close> unexpected '\'
close> expecting '"'

lit> IN: "\x123\&4""
lit> MATCH (COK): "\x123"
lit> VALUE: '\291'

close> IN: "\&4""
close> MATCH (EERR): <EMPTY>
close> ERROR:
close> 1:7:
close> unexpected '\'
close> expecting '"'

lit> IN: "\&4""
lit> MATCH (EERR): <EMPTY>
lit> ERROR:
lit> 1:7:
lit> unexpected '\'
lit> expecting literal character

del> IN: "\&4""
del> MATCH (COK): "\&"
del> VALUE: "\\&"

close> IN: "4""
close> MATCH (EERR): <EMPTY>
close> ERROR:
close> 1:9:
close> unexpected '4'
close> expecting '"'

lit> IN: "4""
lit> MATCH (COK): '4'
lit> VALUE: '4'

close> IN: '"'
close> MATCH (COK): '"'
close> VALUE: '"'

"\291\&4"
```

Step by step, all the parsers we wrapped with `dbg` tell us what happens to
them. It even tells you which continuation the parser follows after every
step.
Consult
[the docs](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:dbg) to
learn what `COK` and `EERR` friends mean.

The addition of `dbg` was inspired
by [this issue](https://github.com/mrkkrp/megaparsec/issues/115), thanks
to [flip111](https://github.com/flip111). (Parsec
users [also want this](https://github.com/aslatter/parsec/issues/63). Maybe
it's worth sending a PR?)

## Conclusion

There are more improvements, but these are the most important ones so far.
See
[the changelog](https://github.com/mrkkrp/megaparsec/blob/master/CHANGELOG.md) for
full list of changes.

Just in case you've never heard of this Megaparsec library before, here are
the links:

* [Megaparsec on Hackage](https://hackage.haskell.org/package/megaparsec)
* [GitHub repo](https://github.com/mrkkrp/megaparsec)
* [Site](https://mrkkrp.github.io/megaparsec/)
* [Tutorials](https://mrkkrp.github.io/megaparsec/tutorials)
