module Types where


import Data.Text (Text)
import Data.Void ( Void )

import Text.Megaparsec
import Text.Megaparsec.Char


type Parser = Parsec Void Text

data Value
  = Integer Int
  | Float Float
  deriving Show

data Function = Function
  { fName :: String -- Haskell is weird with identifier scoping; "function Name"
  , params :: [String]
  , body :: Statement
  }
  deriving Show

data Expr
  = Value Value
  | Variable {name :: String}
  | Call {name :: String, args :: [Expr]}
  | Negation Expr
  | Sum Expr Expr
  | Subtr Expr Expr
  | Product Expr Expr
  | Division Expr Expr
  | Modulo Expr Expr
  | Not Expr
  | And Expr Expr
  | Or Expr Expr
  | Equal Expr Expr
  | NotEqual Expr Expr
  | Less Expr Expr
  | Greater Expr Expr
  | LessEqual Expr Expr
  | GreaterEqual Expr Expr
  deriving Show


data Statement
  = Expr Expr
  | Assign Assignment
  | Block [Statement]
  | IfElse Expr Statement Statement
  | If Expr Statement
  | While Expr Statement
  | Return Expr
  deriving Show

data Assignment = Assignment
    { key :: String
    , value :: Expr
    }
  deriving Show
