{-# LANGUAGE LambdaCase #-}

module Eval where

import Expr (Expr (..))
import Value

import qualified Data.Map as Map


truth :: Value -> Bool
truth (Integer x) = x /= 0
truth (Float y) = y /= 0.0

-- Isn't it funny that this classic C pattern came up?
boolVal :: Bool -> Value
boolVal False = Integer 0
boolVal True = Integer 1

eval :: SymbolTable -> Expr -> Value
eval tbl =
  let eval' = eval tbl
   in \case
        Value v -> v
        Variable name -> case Map.lookup name tbl of
          Nothing -> error "Variable not found."
          Just v -> v
        Call _ _ -> undefined
        Negation x ->
          case eval' x of
            Integer n -> Integer $ negate n
            Float n -> Float $ negate n
        Sum x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> Integer $ x' + y'
            (Float x', Float y') -> Float $ x' + y'
            (Integer x', Float y') -> Float $ fromIntegral x' + y'
            (Float x', Integer y') -> Float $ x' + fromIntegral y'
        Subtr x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> Integer $ x' - y'
            (Float x', Float y') -> Float $ x' - y'
            (Integer x', Float y') -> Float $ fromIntegral x' - y'
            (Float x', Integer y') -> Float $ x' - fromIntegral y'
        Product x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> Integer $ x' * y'
            (Float x', Float y') -> Float $ x' * y'
            (Integer x', Float y') -> Float $ fromIntegral x' * y'
            (Float x', Integer y') -> Float $ x' * fromIntegral y'
        Division x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> Integer $ x' `div` y'
            (Float x', Float y') -> Float $ x' / y'
            (Integer x', Float y') -> Float $ fromIntegral x' / y'
            (Float x', Integer y') -> Float $ x' / fromIntegral y'
        Modulo x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> Integer $ x' `mod` y'
            (_, _) -> error "Modulo must be between two integers."
        Not x -> boolVal (not . truth $ eval' x)
        And x y -> boolVal $ truth (eval' x) && truth (eval' y)
        Or x y -> boolVal $ truth (eval' x) || truth (eval' y)
        Equal x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> boolVal $ x == y
            (Float x', Integer y') -> boolVal $ x == y
            (Integer x', Float y') -> boolVal $ x == y
            (Float x', Float y') -> boolVal $ x == y
        NotEqual x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> boolVal $ x /= y
            (Float x', Integer y') -> boolVal $ x /= y
            (Integer x', Float y') -> boolVal $ x /= y
            (Float x', Float y') -> boolVal $ x /= y
        Greater x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> boolVal $ x > y
            (Float x', Integer y') -> boolVal $ x > y
            (Integer x', Float y') -> boolVal $ x > y
            (Float x', Float y') -> boolVal $ x > y
        Less x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> boolVal $ x < y
            (Float x', Integer y') -> boolVal $ x < y
            (Integer x', Float y') -> boolVal $ x < y
            (Float x', Float y') -> boolVal $ x < y
        GreaterEqual x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> boolVal $ x >= y
            (Float x', Integer y') -> boolVal $ x >= y
            (Integer x', Float y') -> boolVal $ x >= y
            (Float x', Float y') -> boolVal $ x >= y
        LessEqual x y ->
          case (eval' x, eval' y) of
            (Integer x', Integer y') -> boolVal $ x <= y
            (Float x', Integer y') -> boolVal $ x <= y
            (Integer x', Float y') -> boolVal $ x <= y
            (Float x', Float y') -> boolVal $ x <= y
