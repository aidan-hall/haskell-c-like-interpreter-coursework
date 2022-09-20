{-# LANGUAGE RecordWildCards #-}

module Eval where

import Control.Monad.Trans.State.Lazy (StateT, get, put)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Exec (exec)
import SymbolTable (SymbolTable (..), findSymbol)
import Types
  ( Expr (..),
    Function (Function, body, fName, params),
    Value (..),
    returnDefault,
  )

truth :: Value -> Bool
truth (Integer x) = x /= 0
truth (Float y) = y /= 0.0

-- Isn't it funny that this classic C pattern came up?
boolVal :: Bool -> Value
boolVal False = Integer 0
boolVal True = Integer 1

binMathsOp :: (Int -> Int -> Int) -> (Float -> Float -> Float) -> Expr -> Expr -> StateT SymbolTable IO Value
binMathsOp f g x y =
  do
    x' <- eval x
    y' <- eval y
    pure $ case (x', y') of
      (Integer x'', Integer y'') -> Integer $ x'' `f` y''
      (Float x'', Float y'') -> Float $ x'' `g` y''
      (Integer x'', Float y'') -> Float $ fromIntegral x'' `g` y''
      (Float x'', Integer y'') -> Float $ x'' `g` fromIntegral y''

comparisonOp :: (Int -> Int -> Bool) -> (Float -> Float -> Bool) -> Expr -> Expr -> StateT SymbolTable IO Value
comparisonOp f g x y =
  do
    x' <- eval x
    y' <- eval y
    pure $ case (x', y') of
      (Integer x'', Integer y'') -> boolVal $ x'' `f` y''
      (Float x'', Integer y'') -> boolVal $ x'' `g` fromIntegral y''
      (Integer x'', Float y'') -> boolVal $ fromIntegral x'' `g` y''
      (Float x'', Float y'') -> boolVal $ x'' `g` y''



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
        Just Function {..} ->
          do
            argVals <- mapM eval args
            tbl <- get -- Evaluating the arguments could have had an effect; update.
            put
              SymbolTable -- Run function in an isolated scope with arguments assigned.
                { symbols = [Map.fromList $ zip params argVals],
                  functions = functions tbl
                }
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
    Sum x y -> binMathsOp (+) (+) x y
    Subtr x y -> binMathsOp (-) (-) x y
    Product x y -> binMathsOp (*) (*) x y
    Division x y -> binMathsOp div (/) x y
    Modulo x y -> binMathsOp mod (error "Modulo must be between two integers.") x y
    Not x -> eval x >>= \x' -> pure $ boolVal (not . truth $ x')
    And x y -> eval x >>= \x' -> eval y >>= \y' -> pure $ boolVal $ truth x' && truth y'
    Or x y -> eval x >>= \x' -> eval y >>= \y' -> pure $ boolVal $ truth x' || truth y'
    Equal x y -> comparisonOp (==) (==) x y
    NotEqual x y -> comparisonOp (/=) (/=) x y
    Greater x y -> comparisonOp (>) (>) x y

    Less x y -> comparisonOp (<) (<) x y
    GreaterEqual x y -> comparisonOp (>=) (>=) x y
    LessEqual x y -> comparisonOp (<=) (<=) x y
