{-# LANGUAGE RecordWildCards #-}
module SymbolTable where

import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Function

data SymbolTable = SymbolTable
  { symbols :: [Map String Value]
  , functions :: Map String Function
  }
  deriving Show

findSymbol :: String -> SymbolTable -> Maybe Value
findSymbol name SymbolTable {symbols = (m : ms), .. } =
  case Map.lookup name m of
    Just v -> Just v
    Nothing -> findSymbol name SymbolTable {symbols = ms, .. }
findSymbol name SymbolTable {symbols = []} = Nothing


addSymbol :: String -> Value -> SymbolTable -> SymbolTable
-- No scopes: can't add value
addSymbol name value SymbolTable {..} =
  case symbols of
    [] -> SymbolTable [] functions
    (m:ms) -> case findSymbol name SymbolTable {..} of
      Nothing -> SymbolTable {symbols = Map.insert name value m : ms, ..}
      Just _ ->
        let
          -- Replace the symbol at the 'deepest' scope: typical shadowing behaviour.
          replaceExisting name value [] = []
          replaceExisting name value (m:ms) =
            case Map.lookup name m of
              Nothing -> m : replaceExisting name value ms
              Just _ -> Map.insert name value m : ms
        in
          SymbolTable {symbols = replaceExisting name value (m : ms), ..}
