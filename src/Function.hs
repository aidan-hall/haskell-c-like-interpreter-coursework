{-# LANGUAGE RecordWildCards #-}
module Function where

import Text.Megaparsec
import Lex
import Expr
import Types
import Statement

pFunction :: Parser Function
pFunction = do
  fName <- pIdentifier
  params <- lexeme $ parens (sepBy pIdentifier comma)
  body <- pStatement
  pure Function{..}
