---
title: Switch from Parsec to Megaparsec
desc: Practical recommendations for people who decide to switch from Parsec to Megaparsec.
difficulty: 4
date:
  published: October 15, 2015
  updated: September 22, 2017
---

```toc
```

This tutorial explains the practical differences between the two libraries
that you will need to address if you choose to undertake the switch.
Remember, all the functionality available in Parsec is available in
Megaparsec and often in a better form.

## Imports

You'll mainly need to replace `Parsec` part in your imports with
`Megaparsec`. That's pretty simple. Typical import section of module that
uses Megaparsec looks like this:

```haskell
-- this module contains commonly useful tools:
import Text.Megaparsec
-- if you parse a stream of characters
import Text.Megaparsec.Char
-- if you parse a stream of bytes
import Tetx.Megaparsec.Byte
-- if you need to parse permutation phrases:
import Text.Megaparsec.Perm
-- if you need to parse expressions:
import Text.Megaparsec.Expr
-- for lexing of character streams
import qualified Text.Megaparsec.Char.Lexer as L
-- for lexing of binary streams
import qualified Text.Megaparsec.Byte.Lexer as L
```

So, the only noticeable difference that Megaparsec has no
`Text.Megaparsec.Token` module which is replaced with
`Text.Megaparsec.Char.Lexer` (or `Text.Megaparsec.Byte.Lexer` if you work
with binary data), see about this in the section [“What happened to
`Text.Parsec.Token`”](#what-happened-to-text.parsec.token).

## Renamed things

Megaparsec introduces a more consistent naming scheme, so some things are
called differently, but renaming functions is a very easy task, you don't
need to think. Here are renamed items:

* `many1` → `some` (re-exported from `Control.Applicative`)
* `skipMany1` → `skipSome`
* `tokenPrim` → `token`
* `optionMaybe` → `optional` (re-exported from `Control.Applicative`)
* `permute` → `makePermParser`
* `buildExpressionParser` → `makeExprParser`

Character parsing:

* `alphaNum` → `alphaNumChar`
* `digit` → `digitChar`
* `endOfLine` → `eol`
* `hexDigit` → `hexDigitChar`
* `letter` → `letterChar`
* `lower` → `lowerChar`
* `octDigit` → `octDigitChar`
* `space` → `spaceChar` †
* `spaces` → `space` †
* `upper` → `upperChar`

†—pay attention to these, since `space` parses *many* `spaceChar`s,
including zero, if you write something like `many space`, your parser will
hang. So be careful to replace `many space` with either `many spaceChar` or
`spaces`.

## Removed things

Parsec also has many names for the same or similar things. Megaparsec
usually has one function per task. Here are the items that were removed in
Megaparsec and reasons of their removal:

* `parseFromFile`—from file and then parsing its contents is trivial for
  every instance of `Stream` and this function provides no way to use newer
  methods for running a parser, such as `runParser'`.

* `getState`, `putState`, `modifyState`—ad-hoc backtracking user state has
  been eliminated.

* `unexpected`, `token` and `tokens`, now there is a bit different versions
  of these functions under the same names.

* `Reply` and `Consumed` are not public data types anymore, because they are
  low-level implementation details.

* `runPT` and `runP` were essentially synonyms for `runParserT` and
  `runParser` respectively.

* `chainl`, `chainl1`, `chainr`, and `chainr1`—use
  [`Text.Megaparsec.Expr`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Expr.html)
  instead.

## Completely changed things

In Megaparsec 5 and 6 the modules `Text.Megaparsec.Pos` and
`Text.Megaparsec.Error` are completely different from those found in Parsec
and Megaparsec 4. Take some time to look at documentation of the modules if
your use-case requires operations on error messages or positions. You may
like the fact that we have well-typed and extensible error messages now.

## Other

* The `Stream` type class now has `updatePos` method that gives precise
  control over advancing of textual positions during parsing.

* Note that argument order of `label` has been flipped (the label itself
  goes first now), so you can write: `myParser = label "my parser" $ …`.

* Don't use the `label ""` (or the `… <?> ""`) idiom to “hide” some
  “expected” tokens from error messages, use `hidden`.

* New `token` parser is more powerful, its first argument provides full
  control over reported error message while its second argument allows to
  specify how to report a missing token in case of empty input stream.

* New `tokens` parser allows to control how tokens are compared (yes, we
  have case-insensitive `string` called `string'`).

* `unexpected` allows to specify precisely what is unexpected in a
  well-typed manner.

* Tab width is not hard-coded anymore, use `getTabWidth` and `setTabWidth`
  to change it. Default tab width is `defaultTabWidth`.

* Now you can reliably test error messages, equality for them is defined
  properly (in Parsec `Expect "foo"` is equal to `Expect "bar"`).

* To render an error message, apply `parseErrorPretty` or
  `parseErrorPretty'`.

* `count' m n p` allows you to parse from `m` to `n` occurrences of `p`.

* Now we have `someTill` and `eitherP` out of the box (in the package called
  `parse-combinators`).

* `token`-based combinators like `string` and `string'` backtrack by
  default, so it's not necessary to use `try` with them (beginning from
  `4.4.0`). This feature does not affect performance.

* The new `failure` and `fancyFailure` combinators allow to fail with an
  arbitrary error messages that can contain custom data.

For full up to date info see [the
changelog](https://github.com/mrkkrp/megaparsec/blob/master/CHANGELOG.md).
Over the years we have gone so far ahead of Parsec that it would take a lot
of space to enumerate all the nice stuff.

## Character parsing

New character parsers in
[`Text.Megaparsec.Char`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char.html)
may be useful if you work with Unicode:

* `asciiChar`
* `charCategory`
* `controlChar`
* `latin1Char`
* `markChar`
* `numberChar`
* `printChar`
* `punctuationChar`
* `separatorChar`
* `symbolChar`

Case-insensitive character parsers are also available:

* `char'`
* `string'`

## Expression parsing

`makeExprParser` has flipped order of arguments: term parser first, operator
table second. To specify associativity of infix operators you use one of the
three `Operator` constructors:

* `InfixN`—non-associative infix
* `InfixL`—left-associative infix
* `InfixR`—right-associative infix

## What happened to `Text.Parsec.Token`?

That module was extremely inflexible and thus it has been eliminated. In
Megaparsec you have
[`Text.Megaparsec.Char.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Char-Lexer.html)
instead, which doesn't impose anything on user but provides useful helpers.
The module can also parse indentation-sensitive languages.

Let's quickly describe how you go about writing your lexer with
`Text.Megaparsec.Char.Lexer`. First, you should import the module qualified,
we will use `L` as its synonym here.

### White space

Start writing your lexer by defining what counts as *white space* in your
language. `space`, `skipLineComment`, and `skipBlockComment` can be helpful:

```haskell
sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space space1 lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"
```

`sc` is generally called *space consumer*. Often you'll need only one space
consumer, but you can define as many of them as you want. Note that this new
module allows you avoid consuming newline characters automatically, just use
something different than `space1` as first argument of `space`. Even better,
you can control what white space is on per-lexeme basis:

```haskell
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc
```

### Monad transformers

Note that all tools in Megaparsec work with any instance of `MonadParsec`.
All commonly useful monad transformers like `StateT` and `WriterT` are
instances of `MonadParsec` out of the box. For example, what if you want to
collect contents of comments, (say, they are documentation strings of a
sort), you may want to have backtracking user state were you put last
encountered comment satisfying some criteria, and then when you parse
function definition you can check the state and attach doc-string to your
parsed function. It's all possible and easy with Megaparsec:

```haskell
import Control.Monad.State.Lazy

…

type MyParser = StateT String Parser

skipLineComment' :: MyParser ()
skipLineComment' = …

skipBlockComment' :: MyParser ()
skipBlockComment' = …

sc :: MyParser ()
sc = space (void spaceChar) skipLineComment' skipBlockComment'
```

### Indentation-sensitive languages

Parsing of indentation-sensitive language deserves its own tutorial, but
let's take a look at the basic tools upon which we can build. First of all
we should work with space consumer that doesn't eat newlines automatically.
This means we'll need to pick them up manually.

The main helper is called `indentGuard`. It takes a parser that will be used
to consume white space (indentation) and a predicate of the type `Int ->
Bool`. If after running the given parser column number does not satisfy
given predicate, the parser fails with message “incorrect indentation”,
otherwise it returns current column number.

In simple cases you can explicitly pass around value returned by
`indentGuard`, i.e. current level of indentation. If you prefer to preserve
some sort of state you can achieve backtracking state combining `StateT` and
`ParsecT`, like this:

```haskell
StateT Int Parser a
```

Here we have state of the type `Int`. You can use `get` and `put` as usual,
although it may be better to write a modified version of `indentGuard` that
could get current indentation level (indentation level on previous line),
then consume indentation of current line, perform necessary checks, and put
new level of indentation.

*Later update*: now we have full support for indentation-sensitive parsing,
see `nonIndented`, `indentBlock`, and `lineFold` in the
`Text.Megaparsec.Char.Lexer` module.

### Character and string literals

Parsing of string and character literals is done a bit differently than in
Parsec. We have the single helper `charLiteral`, which parses a character
literal. It *does not* parse surrounding quotes, because different languages
may quote character literals differently. The purpose of this parser is to
help with parsing of conventional escape sequences (literal character is
parsed according to rules defined in the Haskell report).

```haskell
charLiteral :: Parser Char
charLiteral = char '\'' *> charLiteral <* char '\''
```

`charLiteral` can be also used to parse string literals. This is simplified
version that will accept plain (not escaped) newlines in string literals
(it's easy to make it conform to Haskell syntax, this is left as an exercise
for the reader):

```haskell
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')
```

### Numbers

Parsing of numbers is easy:

```haskell
integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

number :: Parser Scientific
number lexeme L.scientific -- similar to ‘naturalOrFloat’ in Parsec
```

Megaparsec's numeric parsers have been heavily optimized in version 6, they
are close to Attoparsec's solutions in terms of performance.

Hexadecimal and octal numbers do not parse “0x” or “0o” prefixes, because
different languages may have other prefixes for this sort of numbers. We
should parse the prefixes manually:

```haskell
hexadecimal :: Parser Integer
hexadecimal = lexeme $ char '0' >> char' 'x' >> L.hexadecimal

octal :: Parser Integer
octal = lexeme $ char '0' >> char' 'o' >> L.octal
```

Since the Haskell report says nothing about sign in numeric literals, basic
parsers like `decimal` do not parse sign. You can easily create parsers for
signed numbers with the help of `signed`:

```haskell
signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

signedNumber :: Parser Scientific
signedNumber = L.signed sc number
```

And that's it, shiny and new, `Text.Megaparsec.Char.Lexer` is at your
service, now you can implement anything you want without the need to copy
and edit entire `Text.Parsec.Token` module (people had to do it sometimes,
you know).

## What's next?

Changes you may want to perform may be more fundamental than those described
here. For example, previously you may have to use a workaround because
`Text.Parsec.Token` was not sufficiently flexible. Now you can replace it
with a proper solution. If you want to use the full potential of Megaparsec,
take time to read about its features, they can help you improve your
parsers.
