---
title: Indentation-sensitive parsing
desc: Native, composable solution to identation-sensitivy parsing.
attachment: IndentationSensitiveParsing.hs
difficulty: 3
date:
  published: May 25, 2017
---

Megaparsec 4.3.0 introduces new combinators that should be of some use when
you want to parse indentation-sensitive input. Megaparsec 5.0.0 adds support
for line-folds, completing support for indentation-sensitive parsing. This
tutorial shows how these new tools work, compose, and hopefully, *feel
natural*—something we cannot say about ad-hoc solutions to this problem that
exist as separate packages to work on top of Parsec, for example.

1. [Combinator overview](#combinator-overview)
2. [Parsing a simple indented list](#parsing-a-simple-indented-list)
3. [Nested indented list](#nested-indented-list)
4. [Adding line folds](#adding-line-folds)
5. [Conclusion](#conclusion)

## Combinator overview

From the first release of Megaparsec, there has been the `indentGuard`
function, which is a great shortcut, but a kind of pain to use for complex
tasks. So, we won't cover it here, instead we will talk about the new
combinators built upon it and available beginning from Megaparsec 4.3.0.

First, we have `indentLevel`, which is defined just as:

```haskell
indentLevel :: MonadParsec e s m => m Pos
indentLevel = sourceColumn <$> getPosition
```

That's right, it's just a shortcut, but I found myself using this idiom so
often, so I included it in the public lexer API.

Second, we have `nonIndented`. This allows to make sure that some input is
not indented. Just wrap a parser in `nonIndented` and you're done.

`nonIndented` is trivial to write as well:

```haskell
nonIndented :: MonadParsec e s m
  => m ()              -- ^ How to consume indentation (white space)
  -> m a               -- ^ How to parse actual data
  -> m a
nonIndented sc p = indentGuard sc EQ (unsafePos 1) *> p
```

However, it's a part of a logical model behind high-level parsing of
indentation-sensitive input. We state that there are top-level items that
are not indented (`nonIndented` helps to define parsers for them), and that
all indented tokens are directly or indirectly are “children” of those
top-level definitions. In Megaparsec, we don't need any additional state to
express this. Since indentation is always relative, our idea is to
explicitly tie parsers for “reference” tokens and indented tokens, thus
defining indentation-sensitive grammar via pure combination of parsers, just
like all the other tools in Megaparsec work. This is different from old
solutions built on top of Parsec, where you had to deal with ad-hoc state.
It's also more robust and safer, because the less state you have, the
better.

So, how do you define an indented block? Let's take a look at the signature
of the `indentBlock` helper:

```haskell
indentBlock :: (MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume indentation (white space)
  -> m (IndentOpt m a b) -- ^ How to parse “reference” token
  -> m a
```

First, we specify how to consume indentation. An important thing to note
here is that this space-consuming parser *must* consume newlines as well,
while tokens (“reference” token and indented tokens) should not normally
consume newlines after them.

As you can see, the second argument allows us to parse “reference” token and
return a data structure that tells `indentBlock` what to do next. There are
several options:

```haskell
data IndentOpt m a b
  = IndentNone a
    -- ^ Parse no indented tokens, just return the value
  | IndentMany (Maybe Int) ([b] -> m a) (m b)
    -- ^ Parse many indented tokens (possibly zero), use given indentation
    -- level (if 'Nothing', use level of the first indented token); the
    -- second argument tells how to get final result, and third argument
    -- describes how to parse an indented token
  | IndentSome (Maybe Int) ([b] -> m a) (m b)
    -- ^ Just like 'IndentMany', but requires at least one indented token to
    -- be present
```

We can change our mind and parse no indented tokens, we can parse *many*
(that is, possibly zero) indented tokens or require *at least one* such
token. We can either allow `indentBlock` detect indentation level of the
first indented token and use that, or manually specify indentation level.
This should be flexible enough.

## Parsing a simple indented list

Now it's time to put our new tools into practice. In this section, we will
parse a simple indented list of some items. Let's begin with the import
section:

```haskell
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
```

We will need two kinds of space-consumers: one that consumes new lines `scn`
and one that doesn't `sc` (actually it only parses spaces and tabs here):

```haskell
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
```

Just for fun, we will allow line comments that start with `#` as well.

Assuming `pItemList` parses the entire list, we can define the high-level
parser as:

```haskell
parser :: Parser (String, [String])
parser = pItemList <* eof
```

This will make it consume all input.

`pItemList` is a top-level form that itself is a combination of “reference”
token (header of list) and indented tokens (list items), so:

```haskell
pItemList :: Parser (String, [String]) -- header and list items
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)
```

For our purposes, an item is a sequence of alpha-numeric characters and
dashes:

```haskell
pItem :: Parser String
pItem = lexeme $ some (alphaNumChar <|> char '-')
```

Now, load the code into GHCi and try it with help of `parseTest` built-in:

```haskell
λ> parseTest parser ""
1:1:
unexpected end of input
expecting '-' or alphanumeric character
λ> parseTest parser "something"
("something",[])
λ> parseTest parser "  something"
1:3:
incorrect indentation (got 3, should be equal to 1)
λ> parseTest parser "something\none\ntwo\nthree"
2:1:
unexpected 'o'
expecting end of input
```

Remember that we're using `IndentMany` option, so empty lists are OK, on the
other hand the built-in combinator `space` has hidden the phrase “expecting
more space” from error messages (usually you don't want it because it adds
noise to all messages), so this error message is perfectly reasonable.

Let's continue:

```haskell
λ> parseTest parser "something\n  one\n    two\n  three"
3:5:
incorrect indentation (got 5, should be equal to 3)
λ> parseTest parser "something\n  one\n  two\n three"
4:2:
incorrect indentation (got 2, should be equal to 3)
λ> parseTest parser "something\n  one\n  two\n  three"
("something",["one","two","three"])
```

This definitely seems to work. Let's replace `IndentMany` with `IndentSome`
and `Nothing` with `Just 5` (indentation levels are counted from 1, so it
will require 4 spaces before indented items):

```haskell
pItemList :: Parser (String, [String])
pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentSome (Just (unsafePos 5)) (return . (header, )) pItem)
```

Now:

```haskell
λ> parseTest parser "something\n"
2:1:
incorrect indentation (got 1, should be greater than 1)
λ> parseTest parser "something\n  one"
2:3:
incorrect indentation (got 3, should be equal to 5)
λ> parseTest parser "something\n    one"
("something",["one"])
```

First message may be a bit surprising, but Megaparsec knows that there must
be at least one item in the list, so it checks indentation level and it's 1,
which is incorrect, so it reports it.

## Nested indented list

What I like about `indentBlock` is that another `indentBlock` can be put
inside of it and the whole thing will work smoothly, parsing more complex
input with several levels of indentation. No additional effort is required.

Let's allow list items to have sub-items. For this we will need a new
parser, `pComplexItem` (looks familiar…):

```haskell
pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)
```

A couple of edits to `pItemList` (we're now parsing more complex stuff, so
we need to reflect this in the type signatures):

```haskell
parser :: Parser (String, [(String, [String])])
parser = pItemList <* eof

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

```haskell
Right
  ( "first-chapter"
  , [ ("paragraph-one",   ["note-A","note-B"])
    , ("paragraph-two",   ["note-1","note-2"])
    , ("paragraph-three", []) ] )
```

And this looks like it works!

## Adding line folds

`lineFold` helper is introduced in Megaparsec 5.0.0. A line fold consists of
several elements that can be put on one line or on several lines as long as
indentation level of subsequent items is greater than indentation level of
the first item.

Let's make use of `lineFold` and add line folds to our program.

```haskell
pComplexItem :: Parser (String, [String])
pComplexItem = L.indentBlock scn p
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pLineFold)

pLineFold :: Parser String
pLineFold = L.lineFold scn $ \sc' ->
  let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
  in unwords <$> ps <* sc
```

`lineFold` works like this: you give it a space consumer that accepts
newlines and it gives you a special space consumer that you can use in the
callback to consume space between elements of line fold. An important thing
here is that you should use normal space consumer at the end of line fold or
your fold will have no end.

Playing with the final version of our parser is left as an exercise for the
reader—you can create “items” that consist of multiple words and as long as
they are “line-folded” they will be parsed and concatenated with single
space between them.

## Conclusion

Note that every sub-list behaves independently—you will see that if you try
to feed the parser with various variants of malformed data. And this is no
surprise, since no state is shared between different parts of the
structure—it's just assembled purely from simpler parts—sufficiently elegant
solution in the spirit of the rest of the library.
