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

data Assignment = Assignment
    { name :: String
    , value :: Expr
    }
  deriving (Show, Eq)

data Statement
  = Expr Expr
  | Assign Assignment
  | Block [Statement]
  deriving (Show, Eq)

pAssign :: Parser Assignment
pAssign = do
  name <- pIdentifier
  lexeme $ char '='
  value <- pExpr
  pure Assignment {..}
  

pStatement :: Parser Statement
pStatement =
  Block <$> braces (many pStatement)
  <|> try (Assign <$> (pAssign <* semicolon))
  <|> Expr <$> pExpr <* semicolon
