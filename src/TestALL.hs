module TestALL where

import Debug.Trace
import Formula
import Literal
import NormalForm
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- Test de conversion de Literal à Formula
test_literalToFormula :: (Bool, String)
test_literalToFormula =
    let litA = fromPositive "A"
        litB = fromNegative "B"
        resultA = toFormula litA == Var "A"
        resultB = toFormula litB == Not (Var "B")
    in (resultA && resultB, "Converted: " ++ show (toFormula litA) ++ ", " ++ show (toFormula litB))

-- Test de conversion de Formula à CNF et vice versa
test_formulaToCNFAndBack :: (Bool, String)
test_formulaToCNFAndBack =
    let formula = And (Var "A") (Or (Var "B") (Not (Var "C")))
        cnf = fromFormula formula
    in (toFormulaCNF cnf == formula, "Original: " ++ show formula ++ ", CNF: " ++ show (toFormulaCNF cnf))

-- Test de la taille de CNF
test_cnfSize :: (Bool, String)
test_cnfSize =
    let cnf = CNF (Set.fromList [Set.singleton (fromPositive "A"), Set.singleton (fromNegative "B")])
    in (NormalForm.size cnf == 2, "CNF Size: " ++ show (NormalForm.size cnf))

-- Test de la règle de Robinson
test_robinson :: (Bool, String)
test_robinson =
    let clauseA = Set.singleton (fromPositive "A")
        clauseNotA = Set.singleton (fromNegative "A")
        clauses = Set.fromList [clauseA, clauseNotA]
    in case robinson clauses of
         Just _  -> (True, "Robinson result: exists")
         Nothing -> (False, "Robinson result: does not exist")

-- Test d'évaluation de formules
test_evaluate :: (Bool, String)
test_evaluate = 
    let formula = And (Var "A") (Not (Var "B"))
        env = Map.fromList [("A", True), ("B", False)]
        result = evaluate env formula
    in (result == Just True, "Evaluate result: " ++ show result)

-- Test de vérification de tautologie
test_tautology :: (Bool, String)
test_tautology =
    let formula = Or (Var "A") (Not (Var "A"))
    in (tautology formula, "Tautology check: " ++ show (tautology formula))

-- Test de conversion de Formula à CNF avec une formule plus complexe
test_complexFormulaToCNF :: (Bool, String)
test_complexFormulaToCNF =
    let formula = And (Var "A") (Or (Var "B") (And (Not (Var "C")) (Var "D")))
        cnf = fromFormula formula
    in (toFormulaCNF cnf == formula, "Original: " ++ show formula ++ ", CNF: " ++ show (toFormulaCNF cnf))

-- Test d'évaluation avec une formule plus complexe
test_evaluateComplex :: (Bool, String)
test_evaluateComplex = 
    let formula = Or (And (Var "A") (Var "B")) (And (Not (Var "C")) (Var "D"))
        env = Map.fromList [("A", True), ("B", True), ("C", False), ("D", True)]
        result = evaluate env formula
    in (result == Just True, "Evaluate result: " ++ show result)
    


main :: IO ()
main = do
    mapM_ runTest [ test_literalToFormula
                  , test_formulaToCNFAndBack
                  , test_cnfSize
                  , test_robinson
                  , test_evaluate
                  , test_tautology
                  , test_complexFormulaToCNF
                  , test_evaluateComplex ]

runTest :: (Bool, String) -> IO ()
runTest (result, message) = putStrLn $ message ++ ": " ++ show result

