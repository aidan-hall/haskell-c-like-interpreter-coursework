{-# LANGUAGE RecordWildCards #-}
module Assign where

import qualified Data.Map as Map
import Data.Map (Map)
import Value ( SymbolTable )
import Eval ( eval )
import Expr ( Expr )

data Assignment = Assignment
    { name :: String
    , value :: Expr
    }
  deriving (Show, Eq)

assign :: Assignment -> SymbolTable -> SymbolTable
assign Assignment{..} tbl =
  Map.insert name (eval tbl value) tbl
