module TestNormalForm where

import NormalForm
import Formula (Formula(..))
import Literal
import qualified Data.Set as Set
import Data.Set (Set)

-- Test the conversion from Formula to CNF
testFromFormula :: Bool
testFromFormula = 
    let formula = And (Var "A") (And (Var "B") (Var "C"))
        actualCNF = fromFormula formula
        expectedCNF = CNF $ Set.fromList [Set.singleton (fromPositive "A"),
                                          Set.singleton (fromPositive "B"),
                                          Set.singleton (fromPositive "C")]
    in actualCNF == expectedCNF

-- Test the conversion from CNF to Formula
testToFormula :: Bool
testToFormula = 
    let cnf = CNF $ Set.fromList [Set.singleton (fromPositive "A"),
                                  Set.singleton (fromPositive "B"),
                                  Set.singleton (fromPositive "C")]
        actualFormula = toFormulaCNF cnf
        expectedFormula = And (And (Var "A") (Var "B")) (Var "C")

    in actualFormula == expectedFormula

-- Test Robinson's resolution rule
testRobinson = 
    let cnf = CNF $ Set.fromList [Set.fromList [fromPositive "A", fromNegative "B"],
                                  Set.fromList [fromNegative "A", fromPositive "B"]]
        result = robinson (getCNF cnf)
    in case result of
         Nothing -> False
         Just resolvent -> Set.member Set.empty resolvent


-- Helper function to get the inner value of CNF
getCNF :: CNF -> Set (Set Literal)
getCNF (CNF clauses) = clauses

-- Utility function to run tests and print results
runTests :: IO ()
runTests = do
    putStrLn "Running tests..."
    putStrLn $ "testFromFormula: " ++ (if testFromFormula then "Passed" else "Failed")
    putStrLn $ "testToFormula: " ++ (if testToFormula then "Passed" else "Failed")
    putStrLn $ "testRobinson: " ++ (if testRobinson then "Passed" else "Failed")
    putStrLn "Finished running tests."

main :: IO ()
main = runTests

