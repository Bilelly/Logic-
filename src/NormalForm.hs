-- Module d'exportation
module NormalForm(
    CNF(..), 
    size, 
    toFormulaCNF,  -- Use the new name here
    fromFormula, 
    robinson
) where


import Data.Set (Set)
import qualified Data.Set as Set
import Formula (Formula(..))
import Literal
import Literal hiding (toFormula)
import qualified Literal
import Data.Maybe (catMaybes)
import Data.Maybe (fromMaybe)  -- Add this import


-- Then use Literal.toFormula when referring to the function from the Literal module.



-- | Une forme normale conjonctive
newtype CNF = CNF (Set (Set Literal))
  deriving Eq

instance Show CNF where
  show (CNF clauses) = unwords . map show . Set.toList $ clauses

-- | Taille (nombre de littéraux)
size :: CNF -> Int
size (CNF clauses) = sum . map Set.size $ Set.toList clauses


-- | Conversion de la forme normale en formule logique
toFormulaCNF :: CNF -> Formula
toFormulaCNF (CNF clauses) = foldr And (Const True) $ map clauseToFormula $ Set.toList clauses
  where
    clauseToFormula clause = foldr Or (Const False) $ map Literal.toFormula $ Set.toList clause

-- | Fonction auxiliaire pour convertir une liste de clauses en conjonction
conjunctClauses :: [Set Literal] -> Formula
conjunctClauses []     = error "Aucune clause à conjoncter"  -- ou gérez ce cas comme vous le souhaitez
conjunctClauses [c]    = clauseToFormula c
conjunctClauses (c:cs) = And (clauseToFormula c) (conjunctClauses cs)

-- | Fonction auxiliaire pour convertir une clause en Formula
clauseToFormula :: Set Literal -> Formula
clauseToFormula clause = 
    case Set.toList clause of
      [l] -> Literal.toFormula l
      ls  -> foldr1 Or (map Literal.toFormula ls)


-- | Fonction auxiliaire pour convertir une liste de littéraux en disjonction
disjunctLiterals :: [Literal] -> Formula
disjunctLiterals []     = error "Aucun littéral à disjoncter"  -- ou gérez ce cas comme vous le souhaitez
disjunctLiterals [l]    = literalToFormula l
disjunctLiterals (l:ls) = Or (literalToFormula l) (disjunctLiterals ls)

-- | Fonction auxiliaire pour convertir un littéral en Formula
-- | Fonction auxiliaire pour convertir un littéral en Formula
literalToFormula :: Literal -> Formula
literalToFormula (LitConst b) = Const b
literalToFormula (LitVar v) = Var v
literalToFormula (LitNotVar v) = Not (Var v)


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


-- | Apply ROBINSON's rule on CNF clauses
robinson :: CNF -> CNF
robinson (CNF clauses) = CNF $ fromMaybe clauses (findResolvent clausePairs clauses)
  where
    clausePairs = [(c1, c2) | c1 <- Set.toList clauses, c2 <- Set.toList clauses, c1 /= c2]

    -- | Find a resolvent for a list of clause pairs
    findResolvent :: [(Set Literal, Set Literal)] -> Set (Set Literal) -> Maybe (Set (Set Literal))
    findResolvent [] _ = Nothing
    findResolvent ((c1, c2):rest) acc =
      case resolvent c1 c2 of
        Just clause -> findResolvent rest (Set.insert clause acc)
        Nothing -> findResolvent rest acc

    -- | Find a resolvent for two given clauses
    resolvent :: Set Literal -> Set Literal -> Maybe (Set Literal)
    resolvent c1 c2 =
      let commonLiterals = Set.intersection c1 (Set.map neg c2)
      in if Set.null commonLiterals
         then Nothing
         else Just $ Set.union (Set.difference c1 commonLiterals) (Set.difference c2 (Set.map neg commonLiterals))
