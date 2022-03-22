module Main where

import Eval
import Expr
import Text.Read
import Text.Megaparsec
import Data.Text.Conversions
import System.Environment

import qualified Data.Map as Map

import Value

{- This is the main entry point to your program. -}
main :: IO ()
main = do
  putStrLn "File to evaluate:"
  name <- getLine
  putStrLn name
  source <- convertText <$> readFile name
  case parse (pExpr <* eof) name source of
    Left err -> print err
    Right exp -> print $ eval (Map.fromList [("a", Integer 1)]) exp
