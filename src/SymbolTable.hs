{-# LANGUAGE RecordWildCards #-}

module SymbolTable where

import Data.Map (Map)
import qualified Data.Map as Map
import Types (Function, Value)

data SymbolTable = SymbolTable
  { symbols :: [Map String Value],
    functions :: Map String Function
  }
  deriving (Show)

findSymbol :: String -> SymbolTable -> Maybe Value
findSymbol name SymbolTable {symbols = []} = Nothing
findSymbol name SymbolTable {symbols = (m : ms), ..} =
  case Map.lookup name m of
    Just v -> Just v
    Nothing -> findSymbol name SymbolTable {symbols = ms, ..}

addSymbol :: String -> Value -> SymbolTable -> SymbolTable
addSymbol _ _ SymbolTable {symbols = [], ..} =
  SymbolTable {symbols = [], ..}
addSymbol name value SymbolTable {..} =
  case findSymbol name SymbolTable {..} of
    Nothing -> SymbolTable {symbols = Map.insert name value m : ms, ..}
      where (m:ms) = symbols
    Just _ -> SymbolTable {symbols = replaceExisting name value symbols, ..}
      where
        replaceExisting name value [] = []
        replaceExisting name value (m : ms) =
          case Map.lookup name m of
            Nothing -> m : replaceExisting name value ms
            Just _ -> Map.insert name value m : ms
