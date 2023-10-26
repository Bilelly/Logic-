module TestLiteral where

import Literal
import Formula (Formula(..))  -- Assurez-vous d'avoir exporté les constructeurs nécessaires

testFromBool :: Bool
testFromBool = fromBool True == ConstLit True && fromBool False == ConstLit False

testFromPositive :: Bool
testFromPositive = fromPositive "A" == PositiveLit "A"

testFromNegative :: Bool
testFromNegative = fromNegative "A" == NegativeLit "A"

testNeg :: Bool
testNeg = 
    neg (ConstLit True) == ConstLit False &&
    neg (ConstLit False) == ConstLit True &&
    neg (PositiveLit "A") == NegativeLit "A" &&
    neg (NegativeLit "A") == PositiveLit "A"

testToFormula :: Bool
testToFormula = 
    toFormula (ConstLit True) == Const True &&
    toFormula (PositiveLit "A") == Var "A" &&
    toFormula (NegativeLit "A") == Not (Var "A")

-- Fonction principale pour exécuter tous les tests
runTests :: IO ()
runTests = do
    putStrLn $ "testFromBool: " ++ show testFromBool
    putStrLn $ "testFromPositive: " ++ show testFromPositive
    putStrLn $ "testFromNegative: " ++ show testFromNegative
    putStrLn $ "testNeg: " ++ show testNeg
    putStrLn $ "testToFormula: " ++ show testToFormula

