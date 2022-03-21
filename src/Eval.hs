{-# LANGUAGE LambdaCase #-}

module Eval where

import Expr ( Value(..), Expr(..) )

eval :: Expr -> Value
eval = \case
  Value v -> v
  Variable _ -> undefined
  Negation x ->
    case eval x of
      Integer n -> Integer $ negate n
      Float n -> Float $ negate n
  Sum x y ->
    case (eval x, eval y) of
      (Integer x', Integer y') -> Integer $ x' + y'
      (Float x', Float y') -> Float $ x' + y'
      (Integer x', Float y') -> Float $ fromIntegral x' + y'
      (Float x', Integer y') -> Float $ x' + fromIntegral y'
  Subtr x y ->
    case (eval x, eval y) of
      (Integer x', Integer y') -> Integer $ x' - y'
      (Float x', Float y') -> Float $ x' - y'
      (Integer x', Float y') -> Float $ fromIntegral x' - y'
      (Float x', Integer y') -> Float $ x' - fromIntegral y'
  Product x y ->
    case (eval x, eval y) of
      (Integer x', Integer y') -> Integer $ x' * y'
      (Float x', Float y') -> Float $ x' * y'
      (Integer x', Float y') -> Float $ fromIntegral x' * y'
      (Float x', Integer y') -> Float $ x' * fromIntegral y'
  Division x y ->
    case (eval x, eval y) of
      (Integer x', Integer y') -> Integer $ x' `div` y'
      (Float x', Float y') -> Float $ x' / y'
      (Integer x', Float y') -> Float $ fromIntegral x' / y'
      (Float x', Integer y') -> Float $ x' / fromIntegral y'
  Modulo x y ->
    case (eval x, eval y) of
      (Integer x', Integer y') -> Integer $ x' `mod` y'
      (_, _) -> error "Modulo must be between two integers."
