---
title: Announcing GHC syntax highlighter
desc: Announcing GHC syntax highlighter, a package that allows to highlight Haskell code using lexer of GHC itself.
date:
  published: May 13, 2018
---

Yesterday I published a new package [`ghc-syntax-highlighter`][ghcsh], which
allows to highlight (or to be precise *to tokenize*) Haskell source code
using lexer of GHC itself. Even though I saw a positive reaction on Twitter,
one person (rightly) noted that it's not clear how to use it and docs on
Hackage are missing. This post aims to clarify a few details, demonstrate
example Haskell snippets highlighted using the new package and properly
announce it.

## Motivation and examples

Parsing Haskell is hard, because Haskell is a complex language with
countless features. The only way to get it right 100% is to use parser of
GHC itself. Fortunately, now there is [`ghc`][ghc] package, which as of
version 8.4.1 exports enough of GHC's source code to allow us use its lexer.

Alternative approaches, even decent ones like [`highlight.js`][hljs] either
don't support cutting-edge features or do their work without sufficient
precision so that many tokens end up combined and the end result is
typically still hard to read.

Without further introduction, here is what you get with
[`ghc-syntax-highlighter`][ghcsh]:

```haskell
-- A type family

type family All (c :: k -> Constraint) (xs :: [k]) :: Constraint where
  All c '[]       = ()
  All c (x ': xs) = (c x, All c xs)

-- Explicit forall, type applications and what not

subParser :: forall (name :: Symbol) (names :: [Symbol]) e m a.
  ( KnownSymbol name
  , InSet name names
  , Monad m )
  => FormParser names e m a -- ^ Subparser
  -> FormParser names e m a -- ^ Wrapped parser
subParser p = FormParser $ \v path -> do
  let name = pick @name @names
      f = withObject "form field" (.: unSelectedName name)
      path' = path . (name :)
  case A.parseEither f v of
    Left msg -> do
      let msg' = drop 2 (dropWhile (/= ':') msg)
      return (ParsingFailed (path' []) msg')
    Right v' ->
      unFormParser p v' path'

-- Some more stuff

data Renders b = Renders
  { rWord :: Word -> b
  , rText :: forall l. RLabel l => RText l -> b
  }

equip
  :: forall b. (Word -> b)
  -> (forall l. RLabel l => RText l -> b)
  -> (forall (s :: *). Reifies s (Renders b) => Tagged s b)
  -> b
equip rWord rText f = reify Renders {..} $ \(Proxy :: Proxy s') ->
  unTagged (f :: Tagged s' b)

renderWord :: forall s b. Reifies s (Renders b)
  => Word
  -> Tagged s b
renderWord = Tagged . rWord (reflect (Proxy :: Proxy s))
{-# INLINE renderWord #-}

-- etc.
```

I could not find any flaws. Even syntax highlighting in my editor (Emacs)
works worse. And it's fully static, btw.

## API

The docs on Hackage were missing because Hackage uses GHC 8.2.2 to generate
Haddocks atm, but the library works with GHC 8.4.x only. But I uploaded the
docs today, [here they are][docs].

The API is extremely simple:

```haskell
-- | Token types that are used as tags to mark spans of source code.

data Token
  = KeywordTok         -- ^ Keyword
  | PragmaTok          -- ^ Pragmas
  | SymbolTok          -- ^ Symbols (punctuation that is not an operator)
  | VariableTok        -- ^ Variable name (term level)
  | ConstructorTok     -- ^ Data\/type constructor
  | OperatorTok        -- ^ Operator
  | CharTok            -- ^ Character
  | StringTok          -- ^ String
  | IntegerTok         -- ^ Integer
  | RationalTok        -- ^ Rational number
  | CommentTok         -- ^ Comment (including Haddocks)
  | SpaceTok           -- ^ Space filling
  | OtherTok           -- ^ Something else?
  deriving (Eq, Ord, Enum, Bounded, Show)

-- | Tokenize Haskell source code. If the code cannot be parsed, return
-- 'Nothing'. Otherwise return the original input tagged by 'Token's.
--
-- The parser does not require the input source code to form a valid Haskell
-- program, so as long as the lexer can decompose your input (most of the
-- time), it'll return something in 'Just'.

tokenizeHaskell :: Text -> Maybe [(Token, Text)]
```

So given a simple program:

```haskell
module Main (main) where

import Data.Bits

-- | Program's entry point.

main :: IO ()
main = return ()
```

It outputs something like this:

```haskell
basicModule :: [(Token, Text)] -- taken from the test suite
basicModule =
  [ (KeywordTok,"module")
  , (SpaceTok," ")
  , (ConstructorTok,"Main")
  , (SpaceTok," ")
  , (SymbolTok,"(")
  , (VariableTok,"main")
  , (SymbolTok,")")
  , (SpaceTok," ")
  , (KeywordTok,"where")
  , (SpaceTok,"\n\n")
  , (KeywordTok,"import")
  , (SpaceTok," ")
  , (ConstructorTok,"Data.Bits")
  , (SpaceTok,"\n\n")
  , (CommentTok,"-- | Program's entry point.")
  , (SpaceTok,"\n\n")
  , (VariableTok,"main")
  , (SpaceTok," ")
  , (SymbolTok,"::")
  , (SpaceTok," ")
  , (ConstructorTok,"IO")
  , (SpaceTok," ")
  , (SymbolTok,"(")
  , (SymbolTok,")")
  , (SpaceTok,"\n")
  , (VariableTok,"main")
  , (SpaceTok," ")
  , (SymbolTok,"=")
  , (SpaceTok," ")
  , (VariableTok,"return")
  , (SpaceTok," ")
  , (SymbolTok,"(")
  , (SymbolTok,")")
  , (SpaceTok,"\n")
  ]
```

`Nothing` is rarely returned if ever, because it looks like the lexer is
capable of interpreting almost any text as some stream of GHC tokens.

## How to use it in your blog

Depends on your markdown processor. If you're an [`mmark`][mmark] user, good
news, I just released version 0.2.1.0 of [`mmark-ext`][mmark-ext] which
includes the `ghcSyntaxHighlighter` extension. Due to flexibility of MMark,
I can use this highlighter for Haskell and [`skylighting`][skylighting] as a
fall-back for everything else. [This is][edit] the only edit I had to do to
upgrade Haskell source highlighting for this blog. Consult [the
docs][mmark-ext-docs] for more information.

[skylighting][skylighting] is what Pandoc uses btw. And from what I can tell
it's hardcoded to use only that library for highlighting, so some creativity
may be necessary to get it work.

[ghcsh]: https://hackage.haskell.org/package/ghc-syntax-highlighter
[docs]: https://hackage.haskell.org/package/ghc-syntax-highlighter/docs/GHC-SyntaxHighlighter.html
[ghc]: https://hackage.haskell.org/package/ghc
[hljs]: https://highlightjs.org/
[mmark]: https://hackage.haskell.org/package/mmark
[mmark-ext]: https://hackage.haskell.org/package/mmark-ext
[skylighting]: https://hackage.haskell.org/package/skylighting
[edit]: https://github.com/mrkkrp/markkarpov.com/commit/6977782782ab98e1e79d5e0baa0df78c1ffeccd8#diff-63e7d2cbf23880a9c902c91fa34f6043R451
[mmark-ext-docs]: https://hackage.haskell.org/package/mmark-ext/docs/Text-MMark-Extension-GhcSyntaxHighlighter.html
