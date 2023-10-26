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

-- | A conjunctive normal form
newtype CNF = CNF (Set (Set Literal))
  deriving Eq

instance Show CNF where
  show (CNF clauses) = unwords . map show . Set.toList $ clauses

-- | Size (number of literals)
size :: CNF -> Int
size (CNF clauses) = sum (Set.size <$> Set.toList clauses)

-- | Convert normal form to logical formula
toFormulaCNF :: CNF -> Formula
toFormulaCNF (CNF clauses) = 
  foldl1 And [foldl1 Or [Literal.toFormula lit | lit <- Set.toList clause] | clause <- Set.toList clauses]

-- | Convert logical formula to normal form
fromFormula :: Formula -> CNF
fromFormula formula = 
  let formulaNoImp = eliminateImplication formula
      nnf = toNNF formulaNoImp
      distributed = distribute nnf
  in CNF (toCNF distributed)

-- | Eliminate implications recursively
eliminateImplication :: Formula -> Formula
eliminateImplication (Imp f g) = Or (Not f) g
eliminateImplication (And f1 f2) = And (eliminateImplication f1) (eliminateImplication f2)
eliminateImplication f = f


-- | Convert formula to NNF
toNNF :: Formula -> Formula
toNNF (Not (And f1 f2)) = Or (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Or f1 f2)) = And (toNNF (Not f1)) (toNNF (Not f2))
toNNF (Not (Not f)) = toNNF f
toNNF (And f1 f2) = And (toNNF f1) (toNNF f2)
toNNF (Or f1 f2) = Or (toNNF f1) (toNNF f2)
toNNF f = f

-- | Distribute And over Or
distribute :: Formula -> Formula
distribute (And f1 (Or f2 f3)) = Or (distribute (And f1 f2)) (distribute (And f1 f3))
distribute (And (Or f1 f2) f3) = Or (distribute (And f1 f3)) (distribute (And f2 f3))
distribute (And f1 f2) = And (distribute f1) (distribute f2)
distribute (Or f1 f2) = Or (distribute f1) (distribute f2)
distribute f = f  -- Base case: return the formula if it's a literal or a negation

-- | Convert formula to CNF
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

-- ... (reste du code pour les règles de Robinson)


-- | Apply ROBINSON's rule on clauses
robinson :: Set (Set Literal) -> Maybe (Set (Set Literal))
robinson clauses =
  let pairs = [(c1, c2) | c1 <- Set.toList clauses, c2 <- Set.toList clauses, c1 /= c2] in
  findResolvent pairs clauses


findResolvent :: [(Set Literal, Set Literal)] -> Set (Set Literal) -> Maybe (Set (Set Literal))
findResolvent [] _ = Nothing
findResolvent ((c1, c2):rest) clauses =
  case resolvent c1 c2 of
    Just clause -> Just (Set.insert clause clauses)
    Nothing -> findResolvent rest clauses


resolvent :: Set Literal -> Set Literal -> Maybe (Set Literal)
resolvent c1 c2 =
  let lits = [l | l <- Set.toList c1, neg l `Set.member` c2] in
    if null lits
      then Nothing
      else Just $ foldr Set.delete c1 lits `Set.union` foldr Set.delete c2 (map neg lits)




