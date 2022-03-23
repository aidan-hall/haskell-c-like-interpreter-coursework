{-# LANGUAGE RecordWildCards #-}

module Exec where

import {-# SOURCE #-} Eval ( truth, eval )
import SymbolTable
import Types

import Control.Applicative
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State
import Control.Monad.Trans.Accum
import Control.Monad.IO.Class

import qualified Data.Map as Map

assign :: Assignment -> StateT SymbolTable IO ()
assign Assignment{..} = do
  res <- eval value
  tbl <- get
  put $ addSymbol key res tbl

exec :: Statement -> StateT SymbolTable IO ()
exec statement =
  do
    tbl <- get
    case statement of
      Block ss -> do
          put $ SymbolTable (Map.empty : symbols tbl) (functions tbl) -- Deeper scope.
          execList ss
          tbl' <- get                                         -- Mutated symbol table.
          put $ restored tbl'
          where
            dropped tbl = SymbolTable (drop 1 $ symbols tbl) (functions tbl)
            restored tbl =
              case findSymbol "return" tbl of -- Cascade the return value down.
                Nothing -> dropped tbl
                Just v -> addSymbol "return" v $ dropped tbl
      Assign a -> assign a
      Expr e -> do
        res <- eval e
        liftIO $ print res
      If e s -> do
        res <- eval e
        if truth res
          then exec s
          else pure ()
      IfElse e t f -> do
        res <- eval e
        if truth res
          then exec t
          else exec f
      While e s -> do
        res <- eval e
        if truth res
          then do
          exec s
          exec $ While e s
          else pure ()
      Return e -> do
        assign Assignment { key = "return", value = e }
      

execList :: [Statement] -> StateT SymbolTable IO ()
-- | TODO: Find a more sensible way to do nothing.
execList [] = pure ()
execList (s:ss) = do
  exec s
  tbl <- get
  case findSymbol "return" tbl of
    Nothing -> execList ss
    Just _ -> pure ()
