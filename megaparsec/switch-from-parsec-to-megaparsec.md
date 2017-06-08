---
title: Switch from Parsec to Megaparsec
desc: Practical recommendations for people who decide to switch from Parsec to Megaparsec.
difficulty: 4
date:
  published: October 15, 2015
  updated: June 8, 2017
---

This tutorial explains the practical differences between the two libraries
that you will need to address if you choose to undertake the switch.
Remember, all the functionality available in Parsec is available in
Megaparsec and often in a better form.

1. [Imports](#imports)
2. [Renamed things](#renamed-things)
3. [Removed things](#removed-things)
4. [Completely changed things](#completely-changed-things)
5. [Other](#other)
6. [Character parsing](#character-parsing)
7. [Expression parsing](#expression-parsing)
8. [What happened to `Text.Parsec.Token`?](#what-happened-to-text.parsec.token)
9. [What's next?](#whats-next)

## Imports

You'll mainly need to replace “Parsec” part in your imports with
“Megaparsec”. That's pretty simple. Typical import section of module that
uses Megaparsec looks like this:

```haskell
-- this module contains commonly useful tools:
import Text.Megaparsec
-- this module depends on type of data you want to parse, you only need to
-- import one of these:
import Text.Megaparsec.String          -- if you parse ‘String’
import Text.Megaparsec.ByteString      -- if you parse strict ‘ByteString’
import Text.Megaparsec.ByteString.Lazy -- if you parse lazy ‘ByteString’
import Text.Megaparsec.Text            -- if you parse strict ‘Text’
import Text.Megaparsec.Text.Lazy       -- if you parse lazy ‘Text’
-- if you need to parse permutation phrases:
import Text.Megaparsec.Perm
-- if you need to parse expressions:
import Text.Megaparsec.Expr
-- if you need to parse languages:
import qualified Text.Megaparsec.Lexer as L
```

So, the only noticeable difference that Megaparsec has no
`Text.Megaparsec.Token` module which is replaced with
`Text.Megaparsec.Lexer`, see about this in the
section
[“What happened to `Text.Parsec.Token`”](#what-happened-to-text.parsec.token).

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
usually has one function per task that does its job well. Here are the items
that were removed in Megaparsec and reasons of their removal:

* `parseFromFile`—from file and then parsing its contents is trivial for
  every instance of `Stream` and this function provides no way to use newer
  methods for running a parser, such as `runParser'`.

* `getState`, `putState`, `modifyState`—ad-hoc backtracking user state has
  been eliminated.

* `unexpected`, `token` and `tokens`, now there is a bit different versions
  of these functions under the same name.

* `Reply` and `Consumed` are not public data types anymore, because they are
  low-level implementation details.

* `runPT` and `runP` were essentially synonyms for `runParserT` and
  `runParser` respectively.

* `chainl`, `chainl1`, `chainr`, and `chainr1`—use
  [`Text.Megaparsec.Expr`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Expr.html) instead.

## Completely changed things

In Megaparsec 5 the modules `Text.Megaparsec.Pos` and
`Text.Megaparsec.Error` are completely different from those found in Parsec
and Megaparsec 4. Take some time to look at documentation of the modules if
your use-case requires operations on error messages or positions. You may
like the fact that we have well-typed and extensible error messages now.

## Other

* The `Stream` type class now have `updatePos` method that gives precise
  control over advancing of textual positions during parsing.

* Note that argument order of `label` has been flipped (the label itself
  goes first now), so you can write now: `myParser = label "my parser" $ …`.

* Don't use the `label ""` (or the `… <?> ""`) idiom to “hide” some
  “expected” tokens from error messages, use `hidden`.

* The new `token` parser is more powerful, its first argument provides full
  control over reported error message while its second argument allows to
  specify how to report a missing token in case of empty input stream.

* Now `tokens` parser allows to control how tokens are compared (yes, we
  have case-insensitive `string` called `string'`).

* The `unexpected` parser allows to specify precisely what is unexpected in
  a well-typed manner.

* Tab width is not hard-coded anymore, use `getTabWidth` and `setTabWidth`
  to change it. Default tab width is `defaultTabWidth`.

* Now you can reliably test error messages, equality for them is now defined
  properly (in Parsec `Expect "foo"` is equal to `Expect "bar"`), error
  messages are also well-typed and customizeable.

* To render a error message, apply `parseErrorPretty` on it.

* `count' m n p` allows you to parse from `m` to `n` occurrences of `p`.

* Now you have `someTill` and `eitherP` out of the box.

* `token`-based combinators like `string` and `string'` backtrack by
  default, so it's not necessary to use `try` with them (beginning from
  `4.4.0`). This feature does not affect performance.

* The new `failure` combinator allows to fail with an arbitrary error
  message, it even allows to use your own data types.

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

Ever wanted to have case-insensitive character parsers? Here you go:

* `char'`
* `oneOf'`
* `noneOf'`
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
[`Text.Megaparsec.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Lexer.html)
instead, which doesn't impose anything on user but provides useful
helpers. The module can also parse indentation-sensitive languages.

Let's quickly describe how you go about writing your lexer with
`Text.Megaparsec.Lexer`. First, you should import the module qualified, we
will use `L` as its synonym here.

### White space

Start writing your lexer by defining what counts as *white space* in your
language. `space`, `skipLineComment`, and `skipBlockComment` can be helpful:

```haskell
sc :: Parser () -- ‘sc’ stands for “space consumer”
sc = L.space (void spaceChar) lineComment blockComment
  where lineComment  = L.skipLineComment "//"
        blockComment = L.skipBlockComment "/*" "*/"
```

This is generally called *space consumer*, often you'll need only one space
consumer, but you can define as many of them as you want. Note that this new
module allows you avoid consuming newline characters automatically, just use
something different than `void spaceChar` as first argument of `space`. Even
better, you can control what white space is on per-lexeme basis:

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
let's take a look at the basic tools upon which you can build. First of all
you should work with space consumer that doesn't eat newlines automatically.
This means you'll need to pick them up manually.

The main helper is called `indentGuard`. It takes a parser that will be used
to consume white space (indentation) and a predicate of type `Int -> Bool`.
If after running the given parser column number does not satisfy given
predicate, the parser fails with message “incorrect indentation”, otherwise
it returns current column number.

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
`Text.Megaparsec.Lexer` module.

### Character and string literals

Parsing of string and character literals is done a bit differently than in
Parsec. You have the single helper `charLiteral`, which parses a character
literal. It *does not* parse surrounding quotes, because different languages
may quote character literals differently. Purpose of this parser is to help
with parsing of conventional escape sequences (literal character is parsed
according to rules defined in Haskell report).

```haskell
charLiteral :: Parser Char
charLiteral = char '\'' *> charLiteral <* char '\''
```

Use `charLiteral` to parse string literals. This is simplified version that
will accept plain (not escaped) newlines in string literals (it's easy to
make it conform to Haskell syntax, this is left as an exercise for the
reader):

```haskell
stringLiteral :: Parser String
stringLiteral = char '"' >> manyTill L.charLiteral (char '"')
```

I should note that in `charLiteral` we use built-in support for parsing of
all the tricky combinations of characters. On the other hand Parsec
re-implements the whole thing. Given that it mostly has no tests at all, I
cannot tell for sure that it works.

### Numbers

Parsing of numbers is easy:

```haskell
integer :: Parser Integer
integer = lexeme L.integer

float :: Parser Double
float = lexeme L.float

number :: Parser Scientific
number lexeme L.number -- similar to ‘naturalOrFloat’ in Parsec
```

Note that Megaparsec internally uses the standard Haskell functions to parse
floating point numbers, thus no precision loss is possible (and it's
tested). On the other hand, Parsec again re-implements the whole thing.
Approach taken by Parsec authors is to parse the numbers one by one and then
re-create the floating point number by means of floating point arithmetic.
Any professional knows that this is not possible and the only way to parse
floating point number is via bit-level manipulation (it's usually done on OS
level, in C libraries). Of course results produced by Parsec built-in parser
for floating point numbers are incorrect. This is a known bug now, but it's
been a long time till we “discovered” it, because again, Parsec has no test
suite. (*Update*: it took one year but Parsec's maintainer has recently
merged a pull request that seems to fix that and released Parsec 3.1.11.)

Hexadecimal and octal numbers do not parse “0x” or “0o” prefixes, because
different languages may have other prefixes for this sort of numbers. We
should parse the prefixes manually:

```haskell
hexadecimal :: Parser Integer
hexadecimal = lexeme $ char '0' >> char' 'x' >> L.hexadecimal

octal :: Parser Integer
octal = lexeme $ char '0' >> char' 'o' >> L.octal
```

Since Haskell report says nothing about sign in numeric literals, basic
parsers like `integer` do not parse sign. You can easily create parsers for
signed numbers with the help of `signed`:

```haskell
signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed sc float

signedNumber :: Parser Scientific
signedNumber = L.signed sc number
```

And that's it, shiny and new, `Text.Megaparsec.Lexer` is at your service,
now you can implement anything you want without the need to copy and edit
entire `Text.Parsec.Token` module (people had to do it sometimes, you know).

## What's next?

Changes you may want to perform may be more fundamental than those described
here. For example, previously you may have to use a workaround because
`Text.Parsec.Token` was not sufficiently flexible. Now you can replace it
with a proper solution. If you want to use the full potential of Megaparsec,
take time to read about its features, they can help you improve your
parsers.
