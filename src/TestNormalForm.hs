import Test.HUnit
import NormalForm
import Formula
import Literal
import qualified Data.Set as Set

-- Test for size function
-- Test for size function
testSize :: Test
testSize = TestCase $ do
    let cnf = CNF (Set.fromList [Set.fromList [fromPositive "x", fromNegative "y"], Set.fromList [fromPositive "z"]])
    assertEqual "Size of CNF" 3 (NormalForm.size cnf)  -- Qualify with 'NormalForm.'
    
-- Test for toFormula function
testToFormula :: Test
testToFormula = TestCase $ do
    let cnf = CNF (Set.singleton (Set.fromList [fromPositive "x", fromNegative "y"]))
    assertEqual "Convert CNF to Formula" (And (Or (Var "x") (Not (Var "y"))) (Const True)) (toFormulaCNF cnf)

-- Test for fromFormula function
testFromFormula :: Test
testFromFormula = TestCase $ do
    let formula = And (Var "x") (Not (Var "y"))
    let expectedCNF = CNF (Set.fromList [Set.fromList [fromPositive "x"], Set.fromList [fromNegative "y"]])
    assertEqual "Convert Formula to CNF" expectedCNF (fromFormula formula)



-- Test for robinson function
testRobinson :: Test
testRobinson = TestCase $ do
    let cnf = CNF (Set.fromList [Set.singleton (fromPositive "A"), Set.singleton (fromNegative "A")])
    let expected = CNF Set.empty  -- Expected result after applying Robinson's rule
    assertEqual "Apply Robinson's rule" expected (robinson cnf)

-- Grouping all tests
tests :: Test
tests = TestList [TestLabel "testSize" testSize,
                  TestLabel "testToFormula" testToFormula,
                  TestLabel "testFromFormula" testFromFormula,
                  TestLabel "testRobinson" testRobinson]

-- Main function to run all tests
main :: IO Counts
main = runTestTT tests
