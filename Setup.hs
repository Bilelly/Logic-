import Test.Hspec
import qualified Formula
import qualified Literal
import qualified NormalForm
import qualified Data.Map as Map
import qualified Data.Set as Set  

-- Fonction auxiliaire pour vérifier l'équivalence logique des formules
formulasEquivalent :: Formula.Formula -> Formula.Formula -> Bool
formulasEquivalent (Formula.Or f1 f2) (Formula.Or f3 f4) = (f1 == f3 && f2 == f4) || (f1 == f4 && f2 == f3)
formulasEquivalent f1 f2 = f1 == f2
main :: IO ()
main = hspec $ do

    -- Tests pour le module Formula
    describe "Formula" $ do
        it "devrait évaluer les formules correctement" $ do
            let env = Map.fromList [("a", True), ("b", False)]
            Formula.evaluate env (Formula.And (Formula.Var "a") (Formula.Var "b")) `shouldBe` Just False

        it "devrait gérer les constantes correctement" $ do
            Formula.fromBool True `shouldBe` Formula.Const True

        it "devrait évaluer les formules correctement" $ do
            let env = Map.fromList [("a", True), ("b", False)]
            Formula.evaluate env (Formula.And (Formula.Var "a") (Formula.Var "b")) `shouldBe` Just False
                    
        it "devrait simplifier les formules correctement" $ do
            let formula = Formula.And (Formula.Const True) (Formula.Var "a")
            Formula.simplify formula `shouldBe` Formula.Var "a"

    -- Tests pour le module Literal
    describe "Literal" $ do
        it "devrait convertir de positif correctement" $ do
            Literal.fromPositive "a" `shouldBe` Literal.PositiveLit "a"

        it "devrait convertir les littéraux en formules correctement" $ do
            Literal.toFormula (Literal.fromPositive "a") `shouldBe` Formula.Var "a"


    -- Tests pour le module NormalForm
    describe "NormalForm" $ do
        it "devrait convertir correctement en CNF et inversement" $ do
            let formula = Formula.Or (Formula.Var "a") (Formula.And (Formula.Var "b") (Formula.Var "c"))
            let cnf = NormalForm.fromFormula formula
            NormalForm.toFormulaCNF cnf `shouldBe` Formula.And (Formula.Or (Formula.Var "a") (Formula.Var "b")) (Formula.Or (Formula.Var "a") (Formula.Var "c"))

        it "devrait gérer la taille correctement" $ do
            let cnf = NormalForm.fromFormula (Formula.Or (Formula.Var "a") (Formula.And (Formula.Var "b") (Formula.Var "c")))
            NormalForm.size cnf `shouldBe` 4  

    describe "NormalForm" $ do
        it "convertit correctement en CNF et inversement" $ do
            -- Test 1: Formule simple
            let formula1 = Formula.Or (Formula.Var "a") (Formula.And (Formula.Var "b") (Formula.Var "c"))
            let cnf1 = NormalForm.fromFormula formula1
            NormalForm.toFormulaCNF cnf1 `shouldBe` Formula.And (Formula.Or (Formula.Var "a") (Formula.Var "b")) (Formula.Or (Formula.Var "a") (Formula.Var "c"))

            -- Test 2: Formule avec négation
            let formula2 = Formula.And (Formula.Var "a") (Formula.Not (Formula.Var "b"))
            let cnf2 = NormalForm.fromFormula formula2
            NormalForm.toFormulaCNF cnf2 `shouldBe` Formula.And (Formula.Var "a") (Formula.Not (Formula.Var "b"))

            -- Test 3: Formule avec implication
            let formula3 = Formula.Imp (Formula.Var "a") (Formula.Var "b")
            let cnf3 = NormalForm.fromFormula formula3
            formulasEquivalent (NormalForm.toFormulaCNF cnf3) (Formula.Or (Formula.Not (Formula.Var "a")) (Formula.Var "b")) `shouldBe` True

            -- Test 4: Formule complexe
            let formula4 = Formula.And (Formula.Or (Formula.Var "a") (Formula.Var "b")) (Formula.Not (Formula.Var "c"))
            let cnf4 = NormalForm.fromFormula formula4
            NormalForm.toFormulaCNF cnf4 `shouldBe` Formula.And (Formula.Or (Formula.Var "a") (Formula.Var "b")) (Formula.Not (Formula.Var "c"))

        it "applique correctement la règle de ROBINSON" $ do
            let clause1 = Set.fromList [Literal.fromPositive "a", Literal.fromNegative "b"]
            let clause2 = Set.fromList [Literal.fromNegative "a", Literal.fromPositive "c"]
            let clauses = Set.fromList [clause1, clause2]
            let result = NormalForm.robinson clauses
            let expected = Set.fromList [clause1, clause2, Set.fromList [Literal.fromNegative "b", Literal.fromPositive "c"]]
            result `shouldBe` Just expected

        it "devrait convertir correctement en CNF et inversement" $ do
            let formula = Formula.Or (Formula.Var "a") (Formula.And (Formula.Var "b") (Formula.Var "c"))
            let cnf = NormalForm.fromFormula formula
            NormalForm.toFormulaCNF cnf `shouldBe` Formula.And (Formula.Or (Formula.Var "a") (Formula.Var "b")) (Formula.Or (Formula.Var "a") (Formula.Var "c"))

        it "applique correctement la règle de ROBINSON" $ do

            let clause1 = Set.fromList [Literal.fromPositive "a", Literal.fromNegative "b"]
            let clause2 = Set.fromList [Literal.fromNegative "a", Literal.fromPositive "c"]
            let clauses = Set.fromList [clause1, clause2]
            let result = NormalForm.robinson clauses
            let expected = Set.fromList [clause1, clause2, Set.fromList [Literal.fromNegative "b", Literal.fromPositive "c"]]
            result `shouldBe` Just expected
