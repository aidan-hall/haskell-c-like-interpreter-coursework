module SymbolTable where

import Data.Map (Map)
import qualified Data.Map as Map
import Value

newtype SymbolTable = SymbolTable
  { symbols :: [Map String Value]
  }
  deriving (Show, Eq)

findSymbol :: String -> SymbolTable -> Maybe Value
findSymbol name SymbolTable {symbols = (m : ms)} =
  case Map.lookup name m of
    Just v -> Just v
    Nothing -> findSymbol name SymbolTable {symbols = ms}
findSymbol name SymbolTable {symbols = []} = Nothing


addSymbol :: String -> Value -> SymbolTable -> SymbolTable
-- No scopes: can't add value
addSymbol name value SymbolTable {symbols = []} =
  SymbolTable {symbols = []}
-- There must be at least one scope in this case.
addSymbol name value SymbolTable {symbols = m:ms} =
  case findSymbol name SymbolTable {symbols = m:ms} of
    Nothing -> SymbolTable {symbols = Map.insert name value m : ms}
    Just _ ->
      let
        -- Replace the symbol at the 'deepest' scope: typical shadowing behaviour.
        replaceExisting name value [] = []
        replaceExisting name value (m:ms) =
          case Map.lookup name m of
            Nothing -> m : replaceExisting name value ms
            Just _ -> Map.insert name value m : ms
      in
        SymbolTable {symbols = replaceExisting name value (m : ms)}
