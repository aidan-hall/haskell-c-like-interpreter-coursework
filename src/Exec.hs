{-# LANGUAGE RecordWildCards #-}

module Exec where

import Eval
import Expr (Expr (..))
import Value
import SymbolTable
import Statement

import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State
import Control.Monad.Trans.Accum
import Control.Monad.IO.Class

import qualified Data.Map as Map

assign :: Assignment -> SymbolTable -> SymbolTable
assign Assignment{..} tbl =
  addSymbol name (eval tbl value) tbl

exec :: Statement -> StateT SymbolTable IO ()
exec statement =
  do
    tbl <- get
    case statement of
      Block ss -> do
          put SymbolTable { symbols = Map.fromList [] : symbols tbl} -- Deeper scope.
          execList ss
          tbl' <- get                                         -- Mutated symbol table.
          put SymbolTable { symbols = drop 1 $ symbols tbl' } -- Leaving that scope.
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
