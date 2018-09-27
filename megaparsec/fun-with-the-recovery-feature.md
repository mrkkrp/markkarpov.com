---
title: Fun with the recovery feature
desc: Learn how to skip errors and report multiple parse errors.
attachment: RecoveryFeature.hs
difficulty: 3
date:
  published: February 19, 2016
  updated: September 27, 2018
---

The `withRecovery` primitive allows to recover from parse errors
“on-the-fly” and report several errors after parsing is finished or ignore
them altogether. In this tutorial, we will learn how to use it.

## Language that we will parse

For the purposes of this tutorial, we will write parser for a simple
functional language that consists only of equations with a name on the left
hand side and arithmetic expression on the right hand side of `=`:

```
y = 10
x = 3 * (1 + y)

result = x - 1 # answer is 32
```

Here, it can only calculate arithmetic expressions, but if we were to design
something more powerful, we could introduce more interesting operators to
grab input from console, etc., but since our aim is to explore a new parsing
feature, this language will do.

First, we will write a parser that can parse entire program in this language
as a list of ASTs representing equations. Then we will make it
failure-tolerant in a way, so when it cannot parse a particular equation, it
does not stop, but continues its work until all input is analyzed.

## Parser without recovery

The first version of the parser is very easy to write. We will need the
following imports:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main (main) where

import Control.Applicative (empty)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Scientific (toRealFloat)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
```

To represent AST of our language we will use these definitions:

```haskell
type Program = [Equation]

data Equation = Equation String Expr
  deriving (Eq, Show)

data Expr
  = Value          Double
  | Reference      String
  | Negation       Expr
  | Sum            Expr Expr
  | Subtraction    Expr Expr
  | Multiplication Expr Expr
  | Division       Expr Expr
  deriving (Eq, Show)
```

A program in our language is collection of equations, where every equation
gives a name to an expression which in turn can be simply a number,
reference to other equation, or some math involving those things.

We parse input stream in the form of a `String` and we don't need custom
error messages, so we'll use the following definition of `Parser`:

```haskell
type Parser = Parsec Void String
```

As usual, the first thing that we need to handle is white space. We will
have two space-consuming parsers:

* `scn`—consumes newlines and white space in general. We will use it for
  white space between equations, which will start with a newline (since
  equations are newline-delimited).

* `sc`—this does not consume newlines and is used to define lexemes, i.e.
  things that automatically eat white space after them.

Here is what I've got:

```haskell
lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc
```

Consult Haddocks for description of `L.space`, `L.lexeme`, and `L.symbol`.
In short, `L.space` is a helper to quickly put together a general-purpose
space-consuming parser. We will follow this strategy: *assume no white space
before lexemes and consume all white space after lexemes*. There may be some
white space before the first lexeme, but that will be dealt with specially,
see below.

We also need a parser for equation names (`x`, `y`, and `result` in the
first example). Like in many other programming languages, we will accept
alpha-numeric sequences that do not start with a number:

```haskell
name :: Parser String
name = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "name"
```

To accept integers as well as floating point numbers we use the `scientific`
combinator. It returns `Scientific` number (arbitrary precision space
efficient numbers provided by the
[`scientific`](https://hackage.haskell.org/package/scientific) package)
which we convert to `Double` using the `toRealFloat` function:

```haskell
float :: Parser Double
float = lexeme (Sci.toRealFloat <$> L.scientific)
```

All too easy. Parsing of expressions could slow us down, but there is a
solution out-of-box in the `Control.Monad.Combinators.Expr` module from
([`parser-combinators`](https://hackage.haskell.org/package/parser-combinators)):

```haskell
expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term = parens expr
  <|> (Reference <$> name)
  <|> (Value     <$> float)

table :: [[Operator Parser Expr]]
table =
  [ [Prefix (Negation <$ symbol "-") ]
  , [ InfixL (Multiplication <$ symbol "*")
    , InfixL (Subtraction    <$ symbol "/") ]
  , [ InfixL (Sum            <$ symbol "+")
    , InfixL (Division       <$ symbol "-") ]
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
```

We just wrote a fairly complete parser for expressions in our language! If
you're new to all this stuff I suggest you load the code into GHCi and play
with it a bit. Use the `parseTest` function to feed input into the parser:

```
λ> parseTest expr "5"
Value 5.0
λ> parseTest expr "5 + foo"
Sum (Value 5.0) (Reference "foo")
λ> parseTest expr "(x + y) * 5 + 7 * z"
Sum
  (Multiplication (Sum (Reference "x") (Reference "y")) (Value 5.0))
  (Multiplication (Value 7.0) (Reference "z"))
```

What remains are the parsers for equations and entire program:

```haskell
equation :: Parser Equation
equation = Equation <$> (name <* symbol "=") <*> expr

prog :: Parser Program
prog = between scn eof (sepEndBy equation scn)
```

Note that we need to consume leading white-space in `prog` explicitly. Try
the `prog` parser—it's a complete solution that can parse language we
described in the beginning. Parsing “end of file” `eof` explicitly makes the
parser consume all input and fail loudly if it cannot do it, otherwise it
would just stop on the first problematic token and return what it has parsed
so far.

## Making use of the recovery feature

Our parser is really dandy, it has nice error messages and does its job
well. However, every expression is clearly separated from the others by a
newline. This separation makes it possible to analyze many expressions
independently, even if one of them is malformed, we have no reason to stop
and not to check the others. Reporting multiple parse errors at once may be
a more efficient method of communication with the programmer who needs to
fix them than when she has to recompile the program every time to get to the
next error. In this section we will make our parser failure-tolerant and
able to report multiple error messages at once.

Let's add one more type synonym—`RawData`:

```haskell
type RawData s e = [Either (ParseError s e) Equation]
```

The type represents a collection of equations, just like `Program`, but
every one of them may be malformed: in that case we get an error message in
`Left`, otherwise we have a properly parsed equation in `Right`.

You will be amazed just how easy it is to add recovering to an existing
parser:

```haskell
rawData :: Parser (RawData Char Void)
rawData = between scn eof (sepEndBy e scn)
  where
    e = withRecovery recover (Right <$> equation)
    recover err = Left err <$ manyTill anySingle eol
```

Let try it, here is the input:

```
foo = (x $ y) * 5 + 7.2 * z
bar = 15
```

Result:

```haskell
[ Left
   (TrivialError
     (SourcePos
       { sourceName = ""
       , sourceLine = Pos 1
       , sourceColumn = Pos 10 } :| [])
     (Just (Tokens ('$' :| "")))
     (fromList [ Tokens (')' :| "")
               , Label ('o' :| "perator")
               , Label ('t' :| "he rest of expression") ]))
, Right (Equation "bar" (Value 15.0)) ]
```

How does it work? `withRecovery r p` primitive runs the parser `p` as usual,
but if it fails, it takes its `ParseError` and provides it as an argument to
`r`. In `r`, we start right were `p` failed—no backtracking happens, because
it would make it harder to find position from where to start normal parsing
again. Here we have a chance to consume some input to advance the parser's
textual position. In our case it's as simple as eating all input up to the
next newline, but it might be trickier.

You probably want to know now what happens when recovering parser `r` fails
as well. The answer is: your parser fails as usual, as if no `withRecovery`
was used. It's by design that recovering parser cannot influence error
messages in any way, or it would lead to quite confusing error messages in
some cases, depending on logic of the recovering parser.

Now it's up to you what to do with `RawData`. You can either take all error
messages and print them one by one, or ignore errors altogether and filter
only valid equations to work with.

## Conclusion

When you want to use `withRecovery` the main thing to remember that parts of
text that you want to allow to fail should be clearly separated from each
other, so recovering parser can reliably skip to the next part if the
current part cannot be parsed. In a language like Python, you could use
indentation levels to tell apart high-level definitions. In every case you
should use your judgment and creativity to decide how to make use of
`withRecovery`.
