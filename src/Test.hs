module Test where

import Formula
import Data.Map (fromList)

main :: IO ()
main = do
    test_evaluate
    test_tautology
    test_simplify

test_evaluate :: IO ()
test_evaluate = do
    let env = fromList [("A", True), ("B", False)]
        formula1 = And (Var "A") (Not (Var "B"))
        formula2 = Or (Var "A") (Var "B")
        formula3 = Imp (Var "A") (Var "B")
    putStrLn $ "Evaluation Test 1: " ++ (show $ evaluate env formula1 == Just True)
    putStrLn $ "Evaluation Test 2: " ++ (show $ evaluate env formula2 == Just True)
    putStrLn $ "Evaluation Test 3: " ++ (show $ evaluate env formula3 == Just True)

test_tautology :: IO ()
test_tautology = do
    let formula1 = Or (Var "A") (Not (Var "A"))
        formula2 = And (Var "A") (Not (Var "A"))
    putStrLn $ "Tautology Test 1: " ++ (show $ tautology formula1 == True)
    putStrLn $ "Tautology Test 2: " ++ (show $ tautology formula2 == False)

test_simplify :: IO ()
test_simplify = do
    let formula1 = And (Const True) (Var "A")
        formula2 = Or (Const False) (Var "A")
        formula3 = Not (Not (Var "A"))
    putStrLn $ "Simplification Test 1: " ++ (show $ simplify formula1 == Var "A")
    putStrLn $ "Simplification Test 2: " ++ (show $ simplify formula2 == Var "A")
    putStrLn $ "Simplification Test 3: " ++ (show $ simplify formula3 == Var "A")

