module Eval where

import Types
import SymbolTable
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State

truth :: Value -> Bool

eval :: Expr -> StateT SymbolTable IO Value
