{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Applicative (empty)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Scientific (toRealFloat)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Program = [Equation]

data Equation = Equation String Expr
  deriving (Eq, Show)

data Expr
  = Value Double
  | Reference String
  | Negation Expr
  | Sum Expr Expr
  | Subtraction Expr Expr
  | Multiplication Expr Expr
  | Division Expr Expr
  deriving (Eq, Show)

type Parser = Parsec Void String

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

name :: Parser String
name = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "name"

float :: Parser Double
float = lexeme (toRealFloat <$> L.scientific)

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term =
  parens expr
    <|> (Reference <$> name)
    <|> (Value <$> float)

table :: [[Operator Parser Expr]]
table =
  [ [Prefix (Negation <$ symbol "-")],
    [ InfixL (Multiplication <$ symbol "*"),
      InfixL (Subtraction <$ symbol "/")
    ],
    [ InfixL (Sum <$ symbol "+"),
      InfixL (Division <$ symbol "-")
    ]
  ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

equation :: Parser Equation
equation = Equation <$> (name <* symbol "=") <*> expr

prog :: Parser Program
prog = between scn eof (sepEndBy equation scn)

type RawData s e = [Either (ParseError s e) Equation]

rawData :: Parser (RawData String Void)
rawData = between scn eof (sepEndBy e scn)
  where
    e = withRecovery recover (Right <$> equation)
    recover err = Left err <$ manyTill anySingle eol

main :: IO ()
main = do
  input <- getContents
  parseTest rawData input
