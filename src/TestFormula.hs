import Test.HUnit
import Formula
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Tests for fromBool and fromString
testFromBool :: Test
testFromBool = TestCase $ do
  assertEqual "fromBool True" (Const True) (fromBool True)
  assertEqual "fromBool False" (Const False) (fromBool False)

testFromString :: Test
testFromString = TestCase $ do
  assertEqual "fromString 'x'" (Var "x") (fromString "x")


-- Tests for basic operations (neg, conj, disj, implies)
testNeg :: Test
testNeg = TestCase $ do
  assertEqual "neg (Const True)" (Not (Const True)) (neg (Const True))

testConj :: Test
testConj = TestCase $ do
  assertEqual "conj (Var 'x') (Var 'y')" (And (Var "x") (Var "y")) (conj (Var "x") (Var "y"))

testDisj :: Test
testDisj = TestCase $ do
  assertEqual "disj (Var 'x') (Var 'y')" (Or (Var "x") (Var "y")) (disj (Var "x") (Var "y"))

testImplies :: Test
testImplies = TestCase $ do
  assertEqual "implies (Var 'x') (Var 'y')" (Imp (Var "x") (Var "y")) (implies (Var "x") (Var "y"))


-- Test for isLiteral
testIsLiteral :: Test
testIsLiteral = TestCase $ do
  assertEqual "isLiteral (Const True)" True (isLiteral (Const True))
  assertEqual "isLiteral (Var 'x')" True (isLiteral (Var "x"))
  assertEqual "isLiteral (Not (Var 'x'))" False (isLiteral (Not (Var "x")))


-- Test for evaluate
testEnvironment1 :: Environment
testEnvironment1 = Map.fromList [("x", True), ("y", False)]

testEvaluate :: Test
testEvaluate = TestCase $ do
  assertEqual "evaluate testEnvironment1 (Var 'x')" (Just True) (evaluate testEnvironment1 (Var "x"))
  assertEqual "evaluate testEnvironment1 (And (Var 'x') (Var 'y'))" (Just False) (evaluate testEnvironment1 (And (Var "x") (Var "y")))



-- Test for tautology
testTautology :: Test
testTautology = TestCase $ do
  assertEqual "tautology (Or (Var 'x') (Not (Var 'x')))" True (tautology (Or (Var "x") (Not (Var "x"))))
  assertEqual "tautology (Var 'x')" False (tautology (Var "x"))

testLogicalEquivalence :: Test
testLogicalEquivalence = TestCase $ do
  assertEqual "(Var 'x') <=> (Var 'x')" True ((Var "x") <=> (Var "x"))
  assertEqual "(Var 'x') <=> (Not (Var 'x'))" False ((Var "x") <=> (Not (Var "x")))


-- Test for simplify
testSimplify :: Test
testSimplify = TestCase $ do
  assertEqual "simplify (And (Const True) (Var 'x'))" (Var "x") (simplify (And (Const True) (Var "x")))
  assertEqual "simplify (Or (Const False) (Var 'x'))" (Var "x") (simplify (Or (Const False) (Var "x")))

main :: IO ()
main = do
  runTestTT $ TestList [testFromBool, testFromString, testNeg, testConj, testDisj, testImplies, testIsLiteral, testEvaluate, testSimplify, testTautology, testLogicalEquivalence]
  return ()

