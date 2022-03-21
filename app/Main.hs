{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Lib
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug

type Parser = Parsec Void Text

{- This is the main entry point to your program. -}
main :: IO ()
main = someFunc

mySequence :: Parser (String, String)
mySequence = do
  string "let"
  space
  a <- many alphaNumChar
  space
  string "="
  space
  string "'"
  v <- many alphaNumChar
  string "'"
  space
  optional (string ";")
  space
  eof
  pure (a, v)

whitespace :: Parser ()
whitespace = void $ some (space <|> void newline)

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum Expr Expr
  | Subtr Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | Modulo Expr Expr
  deriving (Eq, Ord, Show)

-- | Parses integers in hexadecimal, octal or decimal format, based on a prefix.
pInteger :: Parser Expr
pInteger =
  Int
    <$> lexeme
      ( (string "0x" *> L.hexadecimal)
          <|> (string "0o" *> L.octal)
          <|> L.decimal
      )

pVariable :: Parser Expr
pVariable =
  Var
    <$> lexeme
      ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pTerm :: Parser Expr
pTerm = choice
  [ parens pExpr
    , pVariable
    , pInteger
    ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
 [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" Product
    , binary "/" Division
    , binary "%" Modulo
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)
