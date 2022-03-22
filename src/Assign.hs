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

assign :: SymbolTable -> Assignment -> SymbolTable
assign tbl Assignment{..} =
  Map.insert name (eval tbl value) tbl
