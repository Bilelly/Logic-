import Test.HUnit
import Formula
import Literal
import NormalForm
import qualified Data.Set as Set
import qualified Formula
import qualified Literal

-- Fonction pour vérifier si un CNF est satisfaisable
isSatisfiable :: CNF -> Bool
isSatisfiable (CNF clauses) = all notContradictory (Set.toList clauses)
  where
    notContradictory clause = not $ any (\l -> Literal.neg l `Set.member` clause) clause

-- Test pour vérifier la satisfaisabilité d'une formule complexe
testIsSatisfiableComplex :: Test
testIsSatisfiableComplex = TestList [
    TestLabel "Complex SAT Formula" $ TestCase $ do
        let formula = Or (And (Var "x") (Var "y")) (And (Not (Var "x")) (Var "z"))  -- (x ∧ y) ∨ (¬x ∧ z)
        let cnf = fromFormula formula
        assertBool "Complex formula should be SAT" (isSatisfiable cnf),

    TestLabel "Complex UNSAT Formula" $ TestCase $ do
        let formula = And (And (Var "x") (Not (Var "x"))) (And (Var "y") (Not (Var "y")))  -- (x ∧ ¬x) ∧ (y ∧ ¬y)
        let cnf = fromFormula formula
        assertBool "Complex formula should be UNSAT" (not $ isSatisfiable cnf)
    ]

-- Main function to run all tests
main :: IO Counts
main = runTestTT testIsSatisfiableComplex
