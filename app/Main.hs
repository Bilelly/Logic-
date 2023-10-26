module Main (main) where

import Formula
import Literal
import NormalForm

main :: IO ()
main = do
    putStrLn "Logical application started."
    
    -- For demonstration purposes, let's construct and evaluate a simple formula
    let f = And (Var "p") (Not (Var "p"))
    putStrLn $ "Evaluating formula: " ++ show f
    print (evaluate undefined f)  -- This requires an environment which I've marked as 'undefined' for now.

