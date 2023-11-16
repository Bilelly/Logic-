{-# LANGUAGE KindSignatures #-}

import Test.HUnit
import Literal
import Formula (Formula(..))

-- Test cases for fromBool
testFromBool :: Test
testFromBool = TestCase $ do
    assertEqual "fromBool True" (LitConst True) (fromBool True)
    assertEqual "fromBool False" (LitConst False) (fromBool False)

-- Test cases for fromPositive
testFromPositive :: Test
testFromPositive = TestCase $ do
    assertEqual "fromPositive 'x'" (LitVar "x") (fromPositive "x")

-- Test cases for fromNegative
testFromNegative :: Test
testFromNegative = TestCase $ do
    assertEqual "fromNegative 'x'" (LitNotVar "x") (fromNegative "x")

-- Test cases for neg
testNeg :: Test
testNeg = TestCase $ do
    assertEqual "neg (LitConst True)" (LitConst False) (neg (LitConst True))
    assertEqual "neg (LitVar 'x')" (LitNotVar "x") (neg (LitVar "x"))
    assertEqual "neg (LitNotVar 'x')" (LitVar "x") (neg (LitNotVar "x"))

-- Test cases for toFormula
testToFormula :: Test
testToFormula = TestCase $ do
    assertEqual "toFormula (LitConst True)" (Const True) (toFormula (LitConst True))
    assertEqual "toFormula (LitVar 'x')" (Var "x") (toFormula (LitVar "x"))
    assertEqual "toFormula (LitNotVar 'x')" (Not (Var "x")) (toFormula (LitNotVar "x"))

-- Grouping all tests
tests :: Test
tests = TestList [TestLabel "testFromBool" testFromBool,
                  TestLabel "testFromPositive" testFromPositive,
                  TestLabel "testFromNegative" testFromNegative,
                  TestLabel "testNeg" testNeg,
                  TestLabel "testToFormula" testToFormula]

-- Main function to run all tests
main :: IO Counts
main = runTestTT tests
