module Main where

import Exec
import SymbolTable
import Statement ( pStatement )
import Text.Megaparsec
import Data.Text.Conversions

import Control.Monad (void)
import Control.Monad.Trans.State.Lazy

import qualified Data.Map as Map

{- This is the main entry point to your program. -}
main :: IO ()
main = do
  putStrLn "File to execute:"
  name <- getLine
  putStrLn name
  source <- convertText <$> readFile name
  case parse (many pStatement <* eof) name source of
    Left err -> print err
    Right exp -> void $ runStateT (execList exp) SymbolTable { symbols = [] } -- No global scope.
