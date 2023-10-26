module Spec where

import Test.Hspec
import Formula
import Literal
import NormalForm

main :: IO ()
main = hspec $ do
    describe "FunctionNameFromSrc" $ do
        it "should return expected result for given input" $ do
            functionName arg1 arg2 `shouldBe` expectedValue

