{-# LANGUAGE LambdaCase #-}

module Exec where

import Eval
import Expr (Expr (..))
import Value
import Assign
import Statement

import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State
import Control.Monad.Trans.Accum
import Control.Monad.IO.Class

import qualified Data.Map as Map

exec :: Statement -> StateT SymbolTable IO ()
exec statement =
  do
    tbl <- get
    case statement of
      Block ss -> execList ss
      Assign a -> do
        modify (assign a)
      Expr e -> do
        liftIO $ print (eval tbl e)

execList :: [Statement] -> StateT SymbolTable IO ()
-- | TODO: Find a more sensible way to do nothing.
execList [] = pure ()
execList (s:ss) = do
  exec s
  execList ss
