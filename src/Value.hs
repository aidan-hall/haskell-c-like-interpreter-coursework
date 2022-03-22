module Value where

import Data.Text (Text)
import Data.Void ( Void )
import qualified Data.Map as Map
import Data.Map (Map)

import Text.Megaparsec
import Text.Megaparsec.Char


data Value
  = Integer Int
  | Float Float
  deriving (Eq, Ord, Show)

type SymbolTable = Map String Value

type Parser = Parsec Void Text
