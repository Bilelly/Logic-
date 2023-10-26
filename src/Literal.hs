{- |
  Module : Literal
  Description : A module representing a literal element in a normal form
  Maintainer  : ???
-}

module Literal(Literal(..), fromBool, fromPositive, fromNegative, neg, toFormula) where

import Data.Kind
import qualified Formula

data Literal
    = ConstLit Bool
    | PositiveLit String
    | NegativeLit String
    deriving (Eq, Ord)

instance Show Literal where
    show (ConstLit b)    = show b
    show (PositiveLit s) = s
    show (NegativeLit s) = "¬" ++ s

fromBool :: Bool -> Literal
fromBool = ConstLit

fromPositive :: String -> Literal
fromPositive = PositiveLit

fromNegative :: String -> Literal
fromNegative = NegativeLit

neg :: Literal -> Literal
neg (ConstLit b)    = ConstLit (not b)
neg (PositiveLit s) = NegativeLit s
neg (NegativeLit s) = PositiveLit s

toFormula :: Literal -> Formula.Formula
toFormula (ConstLit b)    = Formula.Const b
toFormula (PositiveLit s) = Formula.Var s
toFormula (NegativeLit s) = Formula.Not (Formula.Var s)

