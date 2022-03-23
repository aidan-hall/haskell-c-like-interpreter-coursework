{-# LANGUAGE RecordWildCards #-}
module Statement where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Lex
import Expr
import Types

pAssign :: Parser Assignment
pAssign = do
  key <- pIdentifier
  symbol "="
  value <- pExpr
  pure Assignment {..}
  
pIf :: Parser Statement
pIf = do
  void $ symbol "if"
  cond <- lexeme $ parens pExpr
  If cond <$> pStatement

pIfElse :: Parser Statement
pIfElse = do
  If cond whenTrue <- pIf
  void $ symbol "else"
  IfElse cond whenTrue <$> pStatement

pWhile :: Parser Statement
pWhile = do
  void $ symbol "while"
  cond <- lexeme $ parens pExpr
  While cond <$> pStatement

pStatement :: Parser Statement
pStatement =
  Block <$> braces (many pStatement)
  <|> try pIfElse
  <|> pIf
  <|> pWhile
  <|> try (Assign <$> (pAssign <* semicolon))
  <|> Expr <$> pExpr <* semicolon
