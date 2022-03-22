module Main where

import Eval
import Expr
import Exec
import SymbolTable
import Statement
import Text.Read
import Text.Megaparsec
import Data.Text.Conversions
import System.Environment

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad (void)

import qualified Data.Map as Map

import Value

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
