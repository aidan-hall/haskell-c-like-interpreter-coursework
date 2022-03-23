module Main where

import Control.Monad.Trans.State.Lazy (StateT (runStateT))
import qualified Data.Map as Map
import Data.Text.Conversions (convertText)
import Eval (eval)
import Function (pFunction)
import SymbolTable (SymbolTable (SymbolTable, functions, symbols))
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, many, parse)
import Text.Printf (printf)
import Types (Expr (Call), Function (fName))

{- This is the main entry point to your program. -}
main :: IO ()
main = do
  putStrLn "File to execute:"
  name <- getLine
  putStrLn name
  source <- convertText <$> readFile name
  case parse (many pFunction <* eof) name source of
    Left err -> printf $ errorBundlePretty err
    Right exp ->
      let -- Create a map from function names to implementations.
          fTable = Map.fromList $ map (\f -> (fName f, f)) exp
          -- There is no global state, so the symbol table is empty.
          tbl = SymbolTable.SymbolTable {symbols = [], functions = fTable}
       in do
            (val, _) <- runStateT (eval (Call "main" [])) tbl
            putStrLn $ "Exited with value: " ++ show val
