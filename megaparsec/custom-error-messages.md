---
title: How to introduce custom error messages
desc: It's possible to use user-defined data types as part of parse errors, let's learn how.
difficulty: 3
date:
  published: August 10, 2016
  updated: June 8, 2017
---

One of the advantages of Megaparsec 5 is the ability to use your own data
types as part of data that is returned on parse failure. This opens up the
possibility to tailor error messages to your domain of interest in a way
that is quite unique to this library. Needless to say, all data that
constitutes a error message is typed in Megaparsec 5, so it's easy to
inspect and manipulate it.

## The goal

In this tutorial we will walk through creation of a parser found in the
existing library called
[`cassava-megaparsec`](https://hackage.haskell.org/package/cassava-megaparsec),
which is an alternative parser for the
popular [`cassava`](https://hackage.haskell.org/package/cassava) library
that allows to parse CSV data. The default parser features not very
user-friendly error messages, so I was asked to design a better one using
Megaparsec 5.

In addition to the standard error messages (“expected” and “unexpected”
tokens), the library can report problems that have to do with using methods
from `FromRecord` and `FromNamedRecord` type classes that describe how to
transform a collection of `ByteString`s into a particular instance of those
type classes. While performing the conversion, things may go wrong, and we
would like to use a special data constructor in these cases.

The complete source code can be found in
[this GitHub repository](https://github.com/stackbuilders/cassava-megaparsec).

## Language extensions and imports

We will need some language extensions and imports, here is the top of
`Data.Csv.Parser.Megaparsec` almost literally:

```haskell
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

module Data.Csv.Parser.Megaparsec
  ( Cec (..)
  , decode
  , decodeWith
  , decodeByName
  , decodeByNameWith )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Csv hiding
  ( Parser
  , record
  , namedRecord
  , header
  , toNamedRecord
  , decode
  , decodeWith
  , decodeByName
  , decodeByNameWith )
import Data.Data
import Data.Vector (Vector)
import Data.Word (Word8)
import Text.Megaparsec
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Csv              as C
import qualified Data.HashMap.Strict   as H
import qualified Data.Set              as S
import qualified Data.Vector           as V
```

Note that there are two imports for `Data.Csv`, one for some common things
like names of type class that I want to keep unprefixed and the second one
for the rest of the stuff (qualified as `C`).

## What is `ParseError` actually?

To start with custom error messages we should take a look at how parse
errors are represented in Megaparsec 5.

The main type for error messages in `ParseError` which is defined like this:

```haskell
-- | 'ParseError' represents… parse errors. It provides the stack of source
-- positions, a set of expected and unexpected tokens as well as a set of
-- custom associated data. The data type is parametrized over the token type
-- @t@ and the custom data @e@.
--
-- Note that the stack of source positions contains current position as its
-- head, and the rest of positions allows to track full sequence of include
-- files with topmost source file at the end of the list.
--
-- 'Semigroup' (and 'Monoid') instance of the data type allows to merge
-- parse errors from different branches of parsing. When merging two
-- 'ParseError's, the longest match is preferred; if positions are the same,
-- custom data sets and collections of message items are combined.

data ParseError t e = ParseError
  { errorPos        :: NonEmpty SourcePos -- ^ Stack of source positions
  , errorUnexpected :: Set (ErrorItem t)  -- ^ Unexpected items
  , errorExpected   :: Set (ErrorItem t)  -- ^ Expected items
  , errorCustom     :: Set e              -- ^ Associated data, if any
  } deriving (Show, Read, Eq, Data, Typeable, Generic)
```

Conceptually, we have four components in a parse error:

* Position (may be multi-dimensional to support include files).
* Unexpected “items” (see [`ErrorItem`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Error.html#t:ErrorItem) if you are curious).
* Expected “items”.
* Everything else—here we have a set of things of `e` type. `e` is the type
  we will be defining and using in this tutorial.

## Defining a custom error component

We cannot ship the library without some sort of default candidate to take
the place of `e` type, so here it is:

```haskell
-- | “Default error component”. This in our instance of 'ErrorComponent'
-- provided out-of-box.
--
-- @since 5.0.0

data Dec
  = DecFail String         -- ^ 'fail' has been used in parser monad
  | DecIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  deriving (Show, Read, Eq, Ord, Data, Typeable)
```

As you can see it is just a sum type that accounts for all types of failures
that we need to think about in the vanilla Megaparsec:

* `fail` method
* …and incorrect indentation related to the machinery in
  [`Text.Megaparsec.Lexer`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec-Lexer.html).

What this means is that our new custom type should somehow provide a way to
represent those things too. The requirement that a type should be capable of
representing the above-mentioned exceptional situations is captured by the
`ErrorComponent` type class:

```haskell
-- | The type class defines how to represent information about various
-- exceptional situations. Data types that are used as custom data component
-- in 'ParseError' must be instances of this type class.
--
-- @since 5.0.0

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
```

Every type that is going to be used as part of `ParseError` must be an
instance of the `ErrorComponent` type class.

Another thing we would like to do with custom error component is to format
it somehow, so it could be inserted in pretty-printed representation of
`ParseError`. This behavior is defined by the `ShowErrorComponent` type
class:

```haskell
-- | The type class defines how to print custom data component of
-- 'ParseError'.
--
-- @since 5.0.0

class Ord a => ShowErrorComponent a where

  -- | Pretty-print custom data component of 'ParseError'.

  showErrorComponent :: a -> String
```

We will need to make our new data type instance of that class as well.

So, let's start. We can grab existing definitions and instances of `Dec`
data type and change them as necessary. The special case we want to support
is about failed conversion from vector of `ByteString`s to some particular
type, let's capture this:

```haskell
-- | Custom error component for CSV parsing. It allows typed reporting of
-- conversion errors.

data Cec
  = CecFail String
  | CecIndentation Ordering Pos Pos
  | CecConversionError String
  deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent Cec where
  showErrorComponent (CecFail msg) = msg
  showErrorComponent (CecIndentation ord ref actual) =
    "incorrect indentation (got " ++ show (unPos actual) ++
    ", should be " ++ p ++ show (unPos ref) ++ ")"
    where p = case ord of
                LT -> "less than "
                EQ -> "equal to "
                GT -> "greater than "
  showErrorComponent (CecConversionError msg) =
    "conversion error: " ++ msg

instance ErrorComponent Cec where
  representFail        = CecFail
  representIndentation = CecIndentation
```

We have re-used definitions from Megaparsec's source code for `Dec` here and
added a special case represented by `CecConversionError`. It contains a
`String` that conversion functions of Cassava return. We could do better if
Cassava provided typed error values, but `String` is all we have, so let's
work with it.

Another handy definition we need is the `Parser` type synonym. We cannot use
one of the default `Parser` definitions because those assume `Dec`, so we
define it ourselves rather trivially:

```haskell
-- | Parser type that uses “custom error component” 'Cec'.

type Parser = Parsec Cec BL.ByteString
```

## Top level API and helpers

Let's start from the top and take a look at the top-level, public API:

```haskell
-- | Deserialize CSV records form a lazy 'BL.ByteString'. If this fails due
-- to incomplete or invalid input, 'Left' is returned. Equivalent to
-- 'decodeWith' 'defaultDecodeOptions'.

decode :: FromRecord a
  => HasHeader
     -- ^ Whether the data contains header that should be skipped
  -> FilePath
     -- ^ File name (use empty string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseError Char Cec) (Vector a)
decode = decodeWith defaultDecodeOptions

-- | Like 'decode', but lets you customize how the CSV data is parsed.

decodeWith :: FromRecord a
  => DecodeOptions
     -- ^ Decoding options
  -> HasHeader
     -- ^ Whether the data contains header that should be skipped
  -> FilePath
     -- ^ File name (use empty string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseError Char Cec) (Vector a)
decodeWith = decodeWithC csv

-- | Deserialize CSV records from a lazy 'BL.ByteString'. If this fails due
-- to incomplete or invalid input, 'Left' is returned. The data is assumed
-- to be preceded by a header. Equivalent to 'decodeByNameWith'
-- 'defaultDecodeOptions'.

decodeByName :: FromNamedRecord a
  => FilePath          -- ^ File name (use empty string if you have none)
  -> BL.ByteString     -- ^ CSV data
  -> Either (ParseError Char Cec) (Header, Vector a)
decodeByName = decodeByNameWith defaultDecodeOptions

-- | Like 'decodeByName', but lets you customize how the CSV data is parsed.

decodeByNameWith :: FromNamedRecord a
  => DecodeOptions     -- ^ Decoding options
  -> FilePath          -- ^ File name (use empty string if you have none)
  -> BL.ByteString     -- ^ CSV data
  -> Either (ParseError Char Cec) (Header, Vector a)
decodeByNameWith opts = parse (csvWithHeader opts)

-- | Decode CSV data using the provided parser, skipping a leading header if
-- necessary.

decodeWithC
  :: (DecodeOptions -> Parser a)
     -- ^ Parsing function parametrized by 'DecodeOptions'
  -> DecodeOptions
     -- ^ Decoding options
  -> HasHeader
     -- ^ Whether to expect a header in the input
  -> FilePath
     -- ^ File name (use empty string if you have none)
  -> BL.ByteString
     -- ^ CSV data
  -> Either (ParseError Char Cec) a
decodeWithC p opts@DecodeOptions {..} hasHeader = parse parser
  where
    parser = case hasHeader of
      HasHeader -> header decDelimiter *> p opts
      NoHeader  -> p opts
```

Really nothing interesting here, just a bunch of wrappers that boil down to
running the `parser` either with skipping the CSV header or not.

What I would really like to show to you is the helpers, because one of them
is going to be very handy when you decide to write your own parser after
reading this manual. Here are the helpers:

```haskell
-- | End parsing signaling a “conversion error”.

conversionError :: String -> Parser a
conversionError msg = failure S.empty S.empty (S.singleton err)
  where
    err = CecConversionError msg

-- | Convert a 'Record' to a 'NamedRecord' by attaching column names. The
-- 'Header' and 'Record' must be of the same length.

toNamedRecord :: Header -> Record -> NamedRecord
toNamedRecord hdr v = H.fromList . V.toList $ V.zip hdr v

-- | Parse a byte of specified value and return unit.

blindByte :: Word8 -> Parser ()
blindByte = void . char . chr . fromIntegral
```

The `conversionError` is a handy thing to have as you can quickly fail with
your custom error message without writing all the `failure`-related
boilerplate. `toNamedRecord` just converts a `Record` to `NamedRecord`,
while `blindByte` reads a character (passed to it as a `Word8` value) and
returns a unit `()`.

## The parser

Let's start with parsing a field. A field in a CSV file can be either
escaped or unescaped:

```haskell
-- | Parse a field. The field may be in either the escaped or non-escaped
-- format. The returned value is unescaped.

field :: Word8 -> Parser Field
field del = label "field" (escapedField <|> unescapedField del)
```

An escaped field is written inside straight quotes `""` and can contain any
characters at all, but the quote sign itself `"` must be escaped by
repeating it twice:

```haskell
-- | Parse an escaped field.

escapedField :: Parser ByteString
escapedField =
  BC8.pack <$!> between (char '"') (char '"') (many $ normalChar <|> escapedDq)
  where
    normalChar = noneOf "\"" <?> "unescaped character"
    escapedDq  = label "escaped double-quote" ('"' <$ string "\"\"")
```

Simple so far. `unescapedField` is even simpler, it can contain any
character except for the quote sign `"`, delimiter sign, and newline
characters:

```haskell
-- | Parse an unescaped field.

unescapedField :: Word8 -> Parser ByteString
unescapedField del = BC8.pack <$!> many (noneOf es)
  where
    es = chr (fromIntegral del) : "\"\n\r"
```

To parse a record we have to parse a non-empty collection of fields
separated by delimiter characters (supplied from the `DecodeOptions` thing).
Then we convert it to `Vector ByteString`, because that's what Cassava's
conversion functions expect:

```haskell
-- | Parse a record, not including the terminating line separator. The
-- terminating line separate is not included as the last record in a CSV
-- file is allowed to not have a terminating line separator.

record
  :: Word8             -- ^ Field delimiter
  -> (Record -> C.Parser a)
     -- ^ How to “parse” record to get the data of interest
  -> Parser a
record del f = do
  notFollowedBy eof -- to prevent reading empty line at the end of file
  r <- V.fromList <$!> (sepBy1 (field del) (blindByte del) <?> "record")
  case C.runParser (f r) of
    Left msg -> conversionError msg
    Right x  -> return x
```

The `(<$!>)` operator works just like the familiar `(<$>)`operator, but
applies `V.fromList` strictly. Now that we have the vector of `ByteString`s,
we can try to convert it: on success we just return the result, on failure
we fail using the `conversionError` helper.

The library also should handle CSV files with headers:

```haskell
-- | Parse a CSV file that includes a header.

csvWithHeader :: FromNamedRecord a
  => DecodeOptions     -- ^ Decoding options
  -> Parser (Header, Vector a)
     -- ^ The parser that parser collection of named records
csvWithHeader !DecodeOptions {..} = do
  !hdr <- header decDelimiter
  let f = parseNamedRecord . toNamedRecord hdr
  xs   <- sepEndBy1 (record decDelimiter f) eol
  eof
  return $ let !v = V.fromList xs in (hdr, v)

-- | Parse a header, including the terminating line separator.

header :: Word8 -> Parser Header
header del = V.fromList <$!> p <* eol
  where
    p = sepBy1 (name del) (blindByte del) <?> "file header"

-- | Parse a header name. Header names have the same format as regular
-- 'field's.

name :: Word8 -> Parser Name
name del = field del <?> "name in header"
```

The code should be self-explanatory by now. The only thing that remains is
to parse collection of records:

```haskell
-- | Parse a CSV file that does not include a header.

csv :: FromRecord a
  => DecodeOptions     -- ^ Decoding options
  -> Parser (Vector a) -- ^ The parser that parses collection of records
csv !DecodeOptions {..} = do
  xs <- sepEndBy1 (record decDelimiter parseRecord) eol
  eof
  return $! V.fromList xs
```

Too simple!

## Trying it out

The custom error messages play seamlessly with the rest of the parser. Let's
parse a CSV file into collection of `(String, Maybe Int, Double)` items. If
I try to parse `"foo`, I get the usual Megaparsec error message with
“unexpected” and “expected” parts:

```
my-file.csv:1:5:
unexpected end of input
expecting '"', escaped double-quote, or unescaped character
```

However, when that phase of parsing is passed successfully, as with
`foo,12,boo` input, the conversion is attempted and its results are
reported:

```
my-file.csv:1:11:
conversion error: expected Double, got "boo" (Failed reading: takeWhile1)
```

(I wouldn't mind if `(Failed reading: takeWhile1)` part were omitted, but
that's what Cassava's conversion methods are producing.)

## Conclusion

I hope this walk-through has demonstrated that it's quite trivial to insert
your own data into Megaparsec error messages. This way it's also possible to
pump out some data from failing parser or just keep track of things in a
type-safe way, which is one thing we should always care about when writing
Haskell programs.
