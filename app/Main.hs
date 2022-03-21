{-# LANGUAGE RecordWildCards #-}
module Main where

import Lib
import Text.Megaparsec

import Text.Megaparsec.Debug
import Text.Megaparsec.Char
import Control.Monad
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Data.Functor.Identity
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text


{- This is the main entry point to your program. -}
main :: IO ()
main = someFunc

data Primitive
  = PrimInt Integer
  | PrimFloat Float
  | PrimChar Char
  
data MathsOperator = Plus | Minus

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
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc
