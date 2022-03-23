{-# LANGUAGE RecordWildCards #-}

module Eval where

import Types
import SymbolTable
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.State

import Data.Maybe (fromMaybe)

import Exec (exec)

import qualified Data.Map as Map


truth :: Value -> Bool
truth (Integer x) = x /= 0
truth (Float y) = y /= 0.0

-- Isn't it funny that this classic C pattern came up?
boolVal :: Bool -> Value
boolVal False = Integer 0
boolVal True = Integer 1

eval :: Expr -> StateT SymbolTable IO Value
eval expression = do
  tbl <- get
  case expression of
        Value v -> pure v
        Variable name -> case findSymbol name tbl of
          Nothing -> error $ "Variable not found: " ++ show name
          Just v -> pure v
        Call {..} ->
          case Map.lookup name (functions tbl) of
            Nothing -> error $ "Function not found: " ++ show name
            Just Function{..} ->
              do
                argVals <- mapM eval args
                tbl <- get -- Evaluating the arguments could have had an effect; update.

                put SymbolTable -- Run function in an isolated scope with arguments assigned.
                  { symbols = [Map.fromList $ zip params argVals]
                  , functions = functions tbl }
                exec body
                resTable <- get
                put tbl -- Restore the old symbol table.
                pure $ fromMaybe returnDefault (findSymbol "return" resTable)

        Negation x ->
          do
            x' <- eval x
            pure $ case x' of
                     Integer n -> Integer $ negate n
                     Float n -> Float $ negate n
        Sum x y ->
          do
            x' <- eval x
            y' <- eval y
            pure $ case (x', y') of
              (Integer x'', Integer y'') -> Integer $ x'' + y''
              (Float x'', Float y'') -> Float $ x'' + y''
              (Integer x'', Float y'') -> Float $ fromIntegral x'' + y''
              (Float x'', Integer y'') -> Float $ x'' + fromIntegral y''
        Subtr x y ->
          do
            x' <- eval x
            y' <- eval y
            pure $ case (x', y') of
              (Integer x'', Integer y'') -> Integer $ x'' - y''
              (Float x'', Float y'') -> Float $ x'' - y''
              (Integer x'', Float y'') -> Float $ fromIntegral x'' - y''
              (Float x'', Integer y'') -> Float $ x'' - fromIntegral y''
        Product x y ->
          do
            x' <- eval x
            y' <- eval y
            pure $ case (x', y') of
              (Integer x'', Integer y'') -> Integer $ x'' * y''
              (Float x'', Float y'') -> Float $ x'' * y''
              (Integer x'', Float y'') -> Float $ fromIntegral x'' * y''
              (Float x'', Integer y'') -> Float $ x'' * fromIntegral y''
        Division x y ->
          do
            x' <- eval x
            y' <- eval y
            pure $ case (x', y') of
              (Integer x'', Integer y'') -> Integer $ x'' `div` y''
              (Float x'', Float y'') -> Float $ x'' / y''
              (Integer x'', Float y'') -> Float $ fromIntegral x'' / y''
              (Float x'', Integer y'') -> Float $ x'' / fromIntegral y''
        Modulo x y ->
          do
            x' <- eval x
            y' <- eval y
            pure $ case (x', y') of
                     (Integer x'', Integer y'') -> Integer $ x'' `mod` y''
                     (_, _) -> error "Modulo must be between two integers."
        Not x -> eval x >>= \x' -> pure $ boolVal (not . truth $ x')
        And x y -> eval x >>= \x' -> eval y >>= \y' -> pure $ boolVal $ truth x' && truth y'
        Or x y -> eval x >>= \x' -> eval y >>= \y' -> pure $ boolVal $ truth x' || truth y'
        Equal x y ->
          eval x >>= \x' -> eval y >>= \y' ->
          pure $ case (x', y') of
                   (Integer x'', Integer y'') -> boolVal $ x'' == y''
                   (Float x'', Integer y'') -> boolVal $ x'' == fromIntegral y''
                   (Integer x'', Float y'') -> boolVal $ fromIntegral x'' == y''
                   (Float x'', Float y'') -> boolVal $ x'' == y''
        NotEqual x y ->
          eval x >>= \x' -> eval y >>= \y' ->
          pure $ case (x', y') of
                   (Integer x'', Integer y'') -> boolVal $ x'' /= y''
                   (Float x'', Integer y'') -> boolVal $ x'' /= fromIntegral y''
                   (Integer x'', Float y'') -> boolVal $ fromIntegral x'' /= y''
                   (Float x'', Float y'') -> boolVal $ x'' /= y''
        Greater x y ->
          eval x >>= \x' -> eval y >>= \y' ->
          pure $ case (x', y') of
                   (Integer x'', Integer y'') -> boolVal $ x'' > y''
                   (Float x'', Integer y'') -> boolVal $ x'' > fromIntegral y''
                   (Integer x'', Float y'') -> boolVal $ fromIntegral x'' > y''
                   (Float x'', Float y'') -> boolVal $ x'' > y''
        Less x y ->
          eval x >>= \x' -> eval y >>= \y' ->
          pure $ case (x', y') of
                   (Integer x'', Integer y'') -> boolVal $ x'' < y''
                   (Float x'', Integer y'') -> boolVal $ x'' < fromIntegral y''
                   (Integer x'', Float y'') -> boolVal $ fromIntegral x'' < y''
                   (Float x'', Float y'') -> boolVal $ x'' < y''
        GreaterEqual x y ->
          eval x >>= \x' -> eval y >>= \y' ->
          pure $ case (x', y') of
                   (Integer x'', Integer y'') -> boolVal $ x'' >= y''
                   (Float x'', Integer y'') -> boolVal $ x'' >= fromIntegral y''
                   (Integer x'', Float y'') -> boolVal $ fromIntegral x'' >= y''
                   (Float x'', Float y'') -> boolVal $ x'' >= y''
        LessEqual x y ->
          eval x >>= \x' -> eval y >>= \y' ->
          pure $ case (x', y') of
                   (Integer x'', Integer y'') -> boolVal $ x'' <= y''
                   (Float x'', Integer y'') -> boolVal $ x'' <= fromIntegral y''
                   (Integer x'', Float y'') -> boolVal $ fromIntegral x'' <= y''
                   (Float x'', Float y'') -> boolVal $ x'' <= y''
