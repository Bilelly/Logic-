{-# LANGUAGE KindSignatures #-}

module Literal(Literal(..), fromBool, fromPositive, fromNegative, neg, toFormula) where

import Data.Kind
import Formula (Formula(..))

-- | A literal element may be:
-- | * a boolean constant
-- | * a logical variable or the negation of it
data Literal 
    = LitConst Bool         -- A boolean constant
    | LitVar String         -- A logical variable
    | LitNotVar String      -- Negation of a logical variable
    deriving (Eq, Ord)      -- Deriving Eq and Ord

-- Show instance for Literal
instance Show Literal where
  show (LitConst b) = show b
  show (LitVar v) = v
  show (LitNotVar v) = "¬" ++ v

-- | Convert boolean value to constant literal
fromBool :: Bool -> Literal
fromBool b = LitConst b

-- | Convert logical variable to a positive literal
fromPositive :: String -> Literal
fromPositive v = LitVar v

-- | Convert logical variable to a negative literal
fromNegative :: String -> Literal
fromNegative v = LitNotVar v

-- | Negation operation
neg :: Literal -> Literal
neg (LitConst b) = LitConst (not b)
neg (LitVar v) = LitNotVar v
neg (LitNotVar v) = LitVar v

-- | Convert a literal to the corresponding logical formula
toFormula :: Literal -> Formula
toFormula (LitConst b) = Const b
toFormula (LitVar v) = Var v
toFormula (LitNotVar v) = Not (Var v)
