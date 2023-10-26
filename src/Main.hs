module Main where

import Formula
import qualified Data.Map as Map

-- Test pour la fonction 'evaluate'
testEvaluate :: Bool
testEvaluate =
    let env = Map.fromList [("A", True), ("B", False)]
    in all (\(f, v) -> evaluate env f == Just v)
        [ (fromBool True, True)
        , (fromString "A", True)
        , (fromString "B", False)
        , (neg (fromString "A"), False)
        , (conj (fromString "A") (fromString "B"), False)
        , (disj (fromString "A") (fromString "B"), True)
        ]

-- Test pour la fonction 'tautology'
testTautology :: Bool
testTautology =
    let f1 = implies (fromString "A") (fromString "A")  -- A -> A
        f2 = conj (fromString "A") (neg (fromString "A"))  -- A et non A
    in tautology f1 && not (tautology f2)

-- Test pour la fonction 'simplify'
testSimplify :: Bool
testSimplify =
    let f1 = And (Const True) (Var "A")
        f2 = Or (Const False) (Var "B")
        f3 = Not (Not (Var "C"))
    in all (\(f, v) -> simplify f == v)
        [ (f1, Var "A")
        , (f2, Var "B")
        , (f3, Var "C")
        ]

-- Fonction principale pour exécuter tous les tests
main :: IO ()
main = do
    putStrLn $ "testEvaluate: " ++ show testEvaluate
    putStrLn $ "testTautology: " ++ show testTautology
    putStrLn $ "testSimplify: " ++ show testSimplify

