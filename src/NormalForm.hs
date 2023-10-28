-- Module d'exportation
module NormalForm(
    CNF(..), 
    size, 
    toFormulaCNF, 
    fromFormula, 
    robinson
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Formula (Formula(..))
import Literal


-- | Une forme normale conjonctive
newtype CNF = CNF (Set (Set Literal))
  deriving Eq

instance Show CNF where
  show (CNF clauses) = unwords . map show . Set.toList $ clauses

-- | Taille (nombre de littéraux)
size :: CNF -> Int
size (CNF clauses) = sum (Set.size <$> Set.toList clauses)

-- | Conversion de la forme normale en formule logique
toFormulaCNF :: CNF -> Formula
toFormulaCNF (CNF clauses) = foldr1 And (map clauseToFormula (Set.toList clauses))

-- | Fonction auxiliaire pour convertir une liste de clauses en conjonction
conjunctClauses :: [Set Literal] -> Formula
conjunctClauses []     = error "Aucune clause à conjoncter"  -- ou gérez ce cas comme vous le souhaitez
conjunctClauses [c]    = clauseToFormula c
conjunctClauses (c:cs) = And (clauseToFormula c) (conjunctClauses cs)

-- | Fonction auxiliaire pour convertir une clause en Formula
clauseToFormula :: Set Literal -> Formula
clauseToFormula clause = foldr1 Or (map literalToFormula (Set.toList clause))

-- | Fonction auxiliaire pour convertir une liste de littéraux en disjonction
disjunctLiterals :: [Literal] -> Formula
disjunctLiterals []     = error "Aucun littéral à disjoncter"  -- ou gérez ce cas comme vous le souhaitez
disjunctLiterals [l]    = literalToFormula l
disjunctLiterals (l:ls) = Or (literalToFormula l) (disjunctLiterals ls)

-- | Fonction auxiliaire pour convertir un littéral en Formula
literalToFormula :: Literal -> Formula
literalToFormula (PositiveLit n) = Var n
literalToFormula (NegativeLit n) = Not (Var n)

-- | Conversion d'une formule logique en forme normale
fromFormula :: Formula -> CNF
fromFormula formula = 
  let formulaNoImp = eliminateImplication formula
      nnf = toNNF formulaNoImp
      distributed = distribute nnf
  in CNF (toCNF distributed)

-- | Élimination des implications de manière récursive
eliminateImplication :: Formula -> Formula
eliminateImplication (Imp f g) = Or (Not f) g
eliminateImplication (And f1 f2) = And (eliminateImplication f1) (eliminateImplication f2)
eliminateImplication f = f

-- | Conversion de la formule en NNF
toNNF :: Formula -> Formula
toNNF (Not (And f1 f2)) = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Or f1 f2)) = And (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Not f)) = toNNF f
toNNF (And f1 f2) = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2) = Or (toNNF f1) (toNNF f2)
toNNF f = f

-- | Distribution de And sur Or
distribute :: Formula -> Formula
distribute (And f1 (Or f2 f3)) = Or (distribute (And f1 f2)) (distribute (And f1 f3))
distribute (And (Or f1 f2) f3) = Or (distribute (And f1 f3)) (distribute (And f2 f3))
distribute (And f1 f2) = And (distribute f1) (distribute f2)
distribute (Or f1 f2) = Or (distribute f1) (distribute f2)
distribute f = f  -- Cas de base : retourner la formule si c'est un littéral ou une négation

-- | Conversion de la formule en CNF
toCNF :: Formula -> Set (Set Literal)
toCNF formula =
  case formula of
    Const b -> Set.singleton (Set.singleton (fromBool b))
    Var s -> Set.singleton (Set.singleton (fromPositive s))
    Not (Const b) -> Set.singleton (Set.singleton (fromBool (not b)))
    Not (Var s) -> Set.singleton (Set.singleton (fromNegative s))
    And f1 f2 -> Set.union (toCNF f1) (toCNF f2)
    Or f1 f2 ->
      let leftClauses = Set.toList (toCNF f1)
          rightClauses = Set.toList (toCNF f2)
      in Set.fromList [Set.fromList [l | lc <- [lClause, rClause], l <- Set.toList lc] 
                       | lClause <- leftClauses, rClause <- rightClauses]

-- | Appliquer la règle de ROBINSON sur les clauses
robinson :: Set (Set Literal) -> Maybe (Set (Set Literal))
robinson clauses =
  let pairs = [(c1, c2) | c1 <- Set.toList clauses, c2 <- Set.toList clauses, c1 /= c2] in
  findResolvent pairs clauses

-- | Trouver un résolvant
findResolvent :: [(Set Literal, Set Literal)] -> Set (Set Literal) -> Maybe (Set (Set Literal))
findResolvent [] _ = Nothing  -- Cas de base : retourne Nothing si la liste est vide
findResolvent ((c1, c2):rest) clauses =
  case resolvent c1 c2 of
    Just clause -> Just (Set.insert clause clauses)  -- Si un résolvant est trouvé, l'ajoute aux clauses
    Nothing -> findResolvent rest clauses  -- Sinon, continue la recherche avec le reste des paires

-- | Trouver un résolvant pour deux clauses données
resolvent :: Set Literal -> Set Literal -> Maybe (Set Literal)
resolvent c1 c2 =
  let lits = [l | l <- Set.toList c1, neg l `Set.member` c2] in  -- Liste des littéraux qui ont leur négation dans l'autre clause
    if null lits  -- Si la liste est vide, aucun résolvant n'est trouvé
      then Nothing
      else Just $ foldr Set.delete c1 lits `Set.union` foldr Set.delete c2 (map neg lits)  -- Sinon, retourne le résolvant obtenu en supprimant les littéraux et leurs négations des clauses originales

