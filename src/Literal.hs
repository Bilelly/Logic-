-- Module d'exportation
module Literal(
    Literal(..), 
    fromBool, 
    fromPositive, 
    fromNegative, 
    neg, 
    toFormula
) where

-- Importation des dépendances nécessaires
import Data.Kind
import Formula (Formula(..))  -- Assurez-vous que cet import est correct en fonction de la structure de votre projet

-- | Un élément littéral peut être :
-- | * une constante booléenne
-- | * une variable logique ou sa négation
data Literal
    = ConstLit Bool           -- Constante booléenne
    | PositiveLit String      -- Variable logique positive
    | NegativeLit String      -- Variable logique négative
    deriving (Eq, Ord)        -- Dérivation des instances Eq et Ord

-- Instance de Show pour le type Literal
instance Show Literal where
    show (ConstLit b)    = show b
    show (PositiveLit s) = s
    show (NegativeLit s) = "¬" ++ s

-- | Conversion d'une valeur booléenne en littéral constant
fromBool :: Bool -> Literal
fromBool = ConstLit

-- | Conversion d'une variable logique en littéral positif
fromPositive :: String -> Literal
fromPositive = PositiveLit

-- | Conversion d'une variable logique en littéral négatif
fromNegative :: String -> Literal
fromNegative = NegativeLit

-- | Opération de négation
neg :: Literal -> Literal
neg (ConstLit b)    = ConstLit (not b)
neg (PositiveLit s) = NegativeLit s
neg (NegativeLit s) = PositiveLit s

-- | Conversion d'un littéral en la formule logique correspondante
toFormula :: Literal -> Formula
toFormula (ConstLit b)    = Const b
toFormula (PositiveLit s) = Var s
toFormula (NegativeLit s) = Not (Var s)
