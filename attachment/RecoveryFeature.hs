{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Scientific (toRealFloat)
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Lexer as L

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

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space (void spaceChar) lineComment empty

sc :: Parser ()
sc = L.space (void $ oneOf " \t") lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

name :: Parser String
name = lexeme ((:) <$> letterChar <*> many alphaNumChar) <?> "name"

expr :: Parser Expr
expr = makeExprParser term table <?> "expression"

term :: Parser Expr
term = parens expr
  <|> (Reference <$> name)
  <|> (Value     <$> number)

table :: [[Operator Parser Expr]]
table =
  [ [Prefix (Negation <$ symbol "-") ]
  , [ InfixL (Multiplication <$ symbol "*")
    , InfixL (Subtraction    <$ symbol "/") ]
  , [ InfixL (Sum            <$ symbol "+")
    , InfixL (Division       <$ symbol "-") ]
  ]

number :: Parser Double
number = toRealFloat <$> lexeme L.number

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

equation :: Parser Equation
equation = Equation <$> (name <* symbol "=") <*> expr

prog :: Parser Program
prog = between scn eof (sepEndBy equation scn)

type RawData t e = [Either (ParseError t e) Equation]

rawData :: Parser (RawData Char Dec)
rawData = between scn eof (sepEndBy e scn)
  where e = withRecovery recover (Right <$> equation)
        recover err = Left err <$ manyTill anyChar eol

main :: IO ()
main = return ()
