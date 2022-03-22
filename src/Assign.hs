{-# LANGUAGE RecordWildCards #-}
module Assign where

import qualified Data.Map as Map
import Data.Map (Map)
import Value
import Eval

import Statement

assign :: SymbolTable -> Assignment -> SymbolTable
assign tbl Assignment{..} =
  Map.insert name (eval tbl value) tbl
