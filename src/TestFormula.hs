import Test.HUnit
import NormalForm
import Formula
import Literal
import qualified Data.Set as Set

-- Test for size function
testSize :: Test
testSize = TestCase $ do
    let cnf1 = CNF (Set.fromList [Set.fromList [fromPositive "x", fromNegative "y"], Set.fromList [fromPositive "z"]])
    let cnf2 = CNF (Set.fromList [Set.fromList [fromPositive "a"]])
    assertEqual "Size of CNF with multiple literals" 3 (NormalForm.size cnf1)
    assertEqual "Size of CNF with single literal" 1 (NormalForm.size cnf2)

-- Test for fromFormula function
testFromFormula :: Test
testFromFormula = TestCase $ do
    let formula = And (Var "x") (Not (Var "y"))
    let expectedCNF = CNF (Set.fromList [Set.fromList [fromPositive "x"], Set.fromList [fromNegative "y"]])
    assertEqual "Convert Formula to CNF" expectedCNF (fromFormula formula)


-- Test for toFormulaCNF function
testToFormulaCNF :: Test
testToFormulaCNF = TestList [
    TestLabel "Empty CNF" $ TestCase $ do
        let cnf = CNF Set.empty
        let expectedFormula = Const True  -- Un CNF vide est toujours vrai
        assertEqual "Convert empty CNF to Formula" expectedFormula (toFormulaCNF cnf),

    TestLabel "Single Clause CNF" $ TestCase $ do
        let cnf = CNF (Set.singleton (Set.fromList [fromPositive "x", fromNegative "y"]))
        let expectedFormula = Or (Var "x") (Not (Var "y"))  -- Une seule clause est simplement une disjonction
        assertEqual "Convert single clause CNF to Formula" expectedFormula (toFormulaCNF cnf),

    TestLabel "Multiple Clauses CNF" $ TestCase $ do
        let cnf = CNF (Set.fromList [Set.fromList [fromPositive "x", fromNegative "y"], Set.fromList [fromPositive "z"]])
        let expectedFormula = And (Or (Var "x") (Not (Var "y"))) (Var "z")  -- Plusieurs clauses sont reliées par des conjonctions
        assertEqual "Convert multiple clauses CNF to Formula" expectedFormula (toFormulaCNF cnf),

    TestLabel "CNF with Single Literal Clause" $ TestCase $ do
        let cnf = CNF (Set.singleton (Set.singleton (fromPositive "a")))
        let expectedFormula = Var "a"  -- Une clause avec un seul littéral est simplement ce littéral
        assertEqual "Convert single literal clause CNF to Formula" expectedFormula (toFormulaCNF cnf)
    ]


-- Test for Robinson's rule
testRobinson :: Test
testRobinson = TestCase $ do
    let cnf = CNF (Set.fromList [Set.singleton (fromPositive "A"), Set.singleton (fromNegative "A")])
    let actualResult = robinson cnf
    let unexpectedResult = CNF (Set.fromList [Set.singleton (fromPositive "A"), Set.singleton (fromNegative "A")])
    assertBool "Robinson's rule should eliminate contradictory clauses" (actualResult /= unexpectedResult)


-- Grouping all tests
tests :: Test
tests = TestList [TestLabel "testSize" testSize,
                  TestLabel "testFromFormula" testFromFormula,
                  TestLabel "testToFormulaCNF" testToFormulaCNF,
                  TestLabel "testRobinson" testRobinson]

-- Main function to run all tests
main :: IO Counts
main = runTestTT tests
