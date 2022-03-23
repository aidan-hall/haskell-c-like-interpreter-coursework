module Types where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void Text

data Value
  = Integer Int
  | Float Float
  deriving (Show)

data Function = Function
  { fName :: String, -- So-called to avoid conflict with Expr; "function name"
    params :: [String],
    body :: Statement
  }
  deriving (Show)

-- Karpov derivative
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
  deriving (Show)

data Statement
  = Expr Expr
  | Assign Assignment
  | Block [Statement]
  | IfElse Expr Statement Statement
  | If Expr Statement
  | While Expr Statement
  | Return Expr
  deriving (Show)

returnDefault :: Value
returnDefault = Integer 0

data Assignment = Assignment
  { key :: String,
    value :: Expr
  }
  deriving (Show)
