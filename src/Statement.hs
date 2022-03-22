{-# LANGUAGE RecordWildCards #-}
module Statement where

import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Functor.Identity
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Read hiding (choice, parens)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug
import Lex
import Value
import Expr

data Statement
  = Expr Expr
  | Assign Assignment
  | Block [Statement]
  | IfElse Expr Statement Statement
  | If Expr Statement
  | While Expr Statement
  deriving (Show, Eq)

data Assignment = Assignment
    { name :: String
    , value :: Expr
    }
  deriving (Show, Eq)

pAssign :: Parser Assignment
pAssign = do
  name <- pIdentifier
  lexeme $ char '='
  value <- pExpr
  pure Assignment {..}
  
pIf :: Parser Statement
pIf = do
  void $ lexeme (string "if")
  cond <- lexeme $ parens pExpr
  If cond <$> pStatement

pIfElse :: Parser Statement
pIfElse = do
  If cond whenTrue <- pIf
  void $ lexeme (string "else")
  IfElse cond whenTrue <$> pStatement

pWhile :: Parser Statement
pWhile = do
  void $ lexeme (string "while")
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
