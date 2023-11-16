module Formula(Formula(..), fromBool, fromString, neg, conj, disj, implies, isLiteral, has, size, variables, Environment, evaluate, (<=>), tautology, simplify) where

import Data.Kind
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative (liftA2)  -- Add this line

-- Definition of the Formula data type
data Formula 
    = Const Bool                    -- A boolean constant
    | Var String                    -- A logical variable
    | Not Formula                   -- Negation
    | And Formula Formula           -- Conjunction
    | Or Formula Formula            -- Disjunction
    | Imp Formula Formula           -- Implication
    deriving Eq                     -- Deriving instance of Eq


-- Convert a boolean value into a constant logical formula
fromBool :: Bool -> Formula
fromBool b = Const b

-- Convert a variable name into a formula with only the corresponding logical variable
fromString :: String -> Formula
fromString s = Var s

-- Negation operation
neg :: Formula -> Formula
neg f = Not f

-- Conjunction operation (logical "and")
conj :: Formula -> Formula -> Formula
conj f1 f2 = And f1 f2

-- Disjunction operation (logical "or")
disj :: Formula -> Formula -> Formula
disj f1 f2 = Or f1 f2

-- Implication operation
implies :: Formula -> Formula -> Formula
implies f1 f2 = Imp f1 f2

-- Is the formula a literal?
isLiteral :: Formula -> Bool
isLiteral (Const _) = True
isLiteral (Var _) = True
isLiteral _ = False

-- Search for a logical variable in a formula
has :: Formula -> String -> Bool
f `has` v = Set.member v (variables f)

-- Size (number of operators)
size :: Formula -> Int
size (Const _) = 1
size (Var _) = 1
size (Not f) = 1 + size f
size (And f1 f2) = 1 + size f1 + size f2
size (Or f1 f2) = 1 + size f1 + size f2
size (Imp f1 f2) = 1 + size f1 + size f2

-- Retrieve set of all variables occurring in a formula
variables :: Formula -> Set String
variables (Const _) = Set.empty
variables (Var v) = Set.singleton v
variables (Not f) = variables f
variables (And f1 f2) = Set.union (variables f1) (variables f2)
variables (Or f1 f2) = Set.union (variables f1) (variables f2)
variables (Imp f1 f2) = Set.union (variables f1) (variables f2)

-- Show instance for Formula
instance Show Formula where
  show (Const b) = show b
  show (Var v) = v
  show (Not f) = "¬" ++ show f
  show (And f1 f2) = "(" ++ show f1 ++ " ∧ " ++ show f2 ++ ")"
  show (Or f1 f2) = "(" ++ show f1 ++ " ∨ " ++ show f2 ++ ")"
  show (Imp f1 f2) = "(" ++ show f1 ++ " → " ++ show f2 ++ ")"

-- Environment associating logical variables to logical values
type Environment = Map String Bool

-- Evaluation (if possible) of formula in a given environment
-- Evaluate a Formula given an environment
evaluate :: Environment -> Formula -> Maybe Bool
evaluate env (Const b) = Just b
evaluate env (Var v) = Map.lookup v env
evaluate env (Not f) = fmap not (evaluate env f)
evaluate env (And f1 f2) = liftA2 (&&) (evaluate env f1) (evaluate env f2)
evaluate env (Or f1 f2) = liftA2 (||) (evaluate env f1) (evaluate env f2)
evaluate env (Imp f1 f2) = liftA2 impliesFunc (evaluate env f1) (evaluate env f2)
  where
    -- Helper function for logical implication
    impliesFunc :: Bool -> Bool -> Bool
    impliesFunc a b = not a || b

-- Logical equivalence on formulae
(<=>) :: Formula -> Formula -> Bool
f1 <=> f2 = all (\env -> evaluate env f1 == evaluate env f2) allEnvironments
  where
    allVars = Set.toList $ Set.union (variables f1) (variables f2)
    allEnvironments = map Map.fromList $ sequence [ [(v, b) | b <- [True, False]] | v <- allVars ]


-- Is the formula a tautology?
tautology :: Formula -> Bool
tautology f = all (\env -> evaluate env f == Just True) allEnvironments
  where
    allVars = Set.toList $ variables f
    allEnvironments = map Map.fromList $ sequence [ [(v, b) | b <- [True, False]] | v <- allVars ]


-- Attempts to simplify the proposition
simplify :: Formula -> Formula
simplify (And f1 f2) = case (simplify f1, simplify f2) of
    (Const True, f) -> f
    (f, Const True) -> f
    (Const False, _) -> Const False
    (_, Const False) -> Const False
    (f1', f2') -> And f1' f2'
simplify (Or f1 f2) = case (simplify f1, simplify f2) of
    (Const False, f) -> f
    (f, Const False) -> f
    (Const True, _) -> Const True
    (_, Const True) -> Const True
    (f1', f2') -> Or f1' f2'
simplify (Not f) = case simplify f of
    Const b -> Const (not b)
    f' -> Not f'
simplify (Imp f1 f2) = simplify $ Or (Not f1) f2  -- Implication can be expressed as ¬f1 ∨ f2
simplify f = f  -- For constants and variables

