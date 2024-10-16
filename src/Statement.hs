{-# LANGUAGE RecordWildCards #-}

module Statement where

import Control.Monad (void)
import Expr (pExpr, pIdentifier)
import Lex (braces, parens, semicolon, symbol)
import Text.Megaparsec (MonadParsec (try), many, optional, (<|>))
import Types
  ( Assignment (..),
    Expr (Value),
    Parser,
    Statement (..),
    Value (Integer),
    returnDefault
  )

pAssign :: Parser Assignment
pAssign = do
  key <- pIdentifier
  symbol "="
  value <- pExpr
  pure Assignment {..}

pIf :: Parser Statement
pIf = do
  void $ symbol "if"
  cond <- parens pExpr
  If cond <$> pStatement

pIfElse :: Parser Statement
pIfElse = do
  If cond whenTrue <- pIf
  void $ symbol "else"
  IfElse cond whenTrue <$> pStatement

pWhile :: Parser Statement
pWhile = do
  void $ symbol "while"
  cond <- parens pExpr
  While cond <$> pStatement

pReturn :: Parser Statement
pReturn = do
  void $ symbol "return"
  e <- optional pExpr
  semicolon
  (pure . Return) $ case e of
    Nothing -> (Value returnDefault) -- return; is treated as return 0;
    Just e' -> e'

pStatement :: Parser Statement
pStatement =
  Block <$> braces (many pStatement)
    <|> try pIfElse
    <|> pIf
    <|> pWhile
    <|> pReturn
    <|> try (Assign <$> (pAssign <* semicolon))
    <|> Expr <$> pExpr <* semicolon
