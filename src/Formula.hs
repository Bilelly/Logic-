module Formula(Formula(..), fromBool, fromString, neg, conj, disj, implies, isLiteral, has, size, variables, Environment, evaluate, (<=>), tautology, simplify) where


import Data.Kind
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- La structure principale de notre formule
data Formula 
    = Const Bool                    -- Une constante booléenne (True ou False)
    | Var String                    -- Une variable logique
    | Not Formula                   -- Négation
    | And Formula Formula           -- Conjonction (ET logique)
    | Or Formula Formula            -- Disjonction (OU logique)
    | Imp Formula Formula           -- Implication
    deriving (Show, Eq)



-- ** Constructeurs de Formules ** --

fromBool :: Bool -> Formula
fromBool = Const

fromString :: String -> Formula
fromString = Var

-- ** Opérations de Base sur les Formules ** --

neg :: Formula -> Formula
neg = Not

conj :: Formula -> Formula -> Formula
conj = And

disj :: Formula -> Formula -> Formula
disj = Or

implies :: Formula -> Formula -> Formula
implies = Imp

-- ** Propriétés des Formules ** --

isLiteral :: Formula -> Bool
isLiteral (Var _) = True
isLiteral (Not (Var _)) = True
isLiteral _ = False

has :: Formula -> String -> Bool
(Var v) `has` name = v == name
(Not f) `has` name = f `has` name
(And f1 f2) `has` name = f1 `has` name || f2 `has` name
(Or f1 f2) `has` name = f1 `has` name || f2 `has` name
(Imp f1 f2) `has` name = f1 `has` name || f2 `has` name
_ `has` _ = False

size :: Formula -> Int
size (Var _) = 0
size (Not f) = 1 + size f
size (And f1 f2) = 1 + size f1 + size f2
size (Or f1 f2) = 1 + size f1 + size f2
size (Imp f1 f2) = 1 + size f1 + size f2
size (Const _) = 0

variables :: Formula -> Set String
variables (Var v) = Set.singleton v
variables (Not f) = variables f
variables (And f1 f2) = Set.union (variables f1) (variables f2)
variables (Or f1 f2) = Set.union (variables f1) (variables f2)
variables (Imp f1 f2) = Set.union (variables f1) (variables f2)
variables (Const _) = Set.empty


-- Type pour les environnements (associations variable/valeur)
type Environment = Map String Bool

-- Évaluation d'une formule dans un environnement donné
evaluate :: Environment -> Formula -> Maybe Bool
evaluate env (Const b) = Just b
evaluate env (Var v) = Map.lookup v env
evaluate env (Not f) = not <$> evaluate env f
evaluate env (And f1 f2) = do
    b1 <- evaluate env f1
    b2 <- evaluate env f2
    return (b1 && b2)
evaluate env (Or f1 f2) = do
    b1 <- evaluate env f1
    b2 <- evaluate env f2
    return (b1 || b2)
evaluate env (Imp f1 f2) = do
    b1 <- evaluate env f1
    b2 <- evaluate env f2
    return (not b1 || b2)

-- Équivalence logique
(<=>) :: Formula -> Formula -> Bool
f <=> g = all (\env -> eval f env == eval g env) envs
  where
    vars = Set.union (variables f) (variables g)
    envs = allEnvironments $ Set.toList vars
    eval formula env = case evaluate (Map.fromList env) formula of
      Just val -> val
      Nothing  -> error "Incomplete environment"

allEnvironments :: [String] -> [[(String, Bool)]]
allEnvironments [] = [[]]
allEnvironments (v:vs) =
  [(v, True) : env | env <- allEnvironments vs] ++
  [(v, False) : env | env <- allEnvironments vs]

-- Vérifier si la formule est une tautologie
tautology :: Formula -> Bool
tautology f = all (\env -> eval f env) envs
  where
    vars = variables f
    envs = allEnvironments $ Set.toList vars
    eval formula env = case evaluate (Map.fromList env) formula of
      Just val -> val
      Nothing  -> error "Incomplete environment"

-- Simplification (cette version est rudimentaire)
simplify :: Formula -> Formula
simplify (And (Const True) f) = simplify f
simplify (And f (Const True)) = simplify f
simplify (And (Const False) _) = Const False
simplify (And _ (Const False)) = Const False
simplify (Or (Const True) _) = Const True
simplify (Or _ (Const True)) = Const True
simplify (Or (Const False) f) = simplify f
simplify (Or f (Const False)) = simplify f
simplify (Not (Const True)) = Const False
simplify (Not (Const False)) = Const True
simplify (Not (Not f)) = simplify f
simplify (Imp f1 f2) = simplify (Or (Not f1) f2)
simplify f = f  -- Pour les autres cas, on ne simplifie pas


