module Value where

import Data.Text (Text)
import Data.Void ( Void )

import Text.Megaparsec
import Text.Megaparsec.Char


data Value
  = Integer Int
  | Float Float
  deriving (Eq, Ord, Show)

type Parser = Parsec Void Text
