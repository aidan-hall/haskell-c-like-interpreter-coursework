module Main where

import Exec
import Eval
import SymbolTable
import Statement ( pStatement )
import Function
import Types
import Text.Megaparsec
import Data.Text.Conversions

import Control.Monad (void)
import Control.Monad.Trans.State.Lazy
import Control.Monad.IO.Class

import qualified Data.Map as Map

{- This is the main entry point to your program. -}
main :: IO ()
main = do
  putStrLn "File to execute:"
  name <- getLine
  putStrLn name
  source <- convertText <$> readFile name
  case parse (many pFunction <* eof) name source of
    Left err -> print err
    Right exp ->
      let
        fTable = Map.fromList $ map (\f -> (fName f, f)) exp
        tbl = SymbolTable { symbols = [], functions = fTable }
      in
        do
          (val, _) <- runStateT (eval (Call "main" [])) tbl
          print val
