-- Importation des modules nécessaires
import Test.HUnit
import NormalForm
import Formula
import Literal
import qualified Data.Set as Set

-- Test pour vérifier la taille d'une forme normale conjonctive (CNF)
testSize :: Test
testSize = TestCase $ do
    let cnf1 = CNF (Set.fromList [Set.fromList [fromPositive "x", fromNegative "y"], Set.fromList [fromPositive "z"]])
    let cnf2 = CNF (Set.singleton (Set.singleton (fromPositive "a")))
    assertEqual "Size of CNF with multiple literals" 3 (NormalForm.size cnf1)
    assertEqual "Size of CNF with single literal" 1 (NormalForm.size cnf2)

-- Test pour convertir une CNF en formule

testToFormulaCNF :: Test
testToFormulaCNF = TestList [
    -- Tests existants
    TestLabel "Empty CNF to Formula" $ TestCase $ do
        let cnf = CNF Set.empty
        assertEqual "Convert empty CNF to Formula" (Const True) (toFormulaCNF cnf),

    TestLabel "Single Clause CNF to Formula" $ TestCase $ do
        let cnf = CNF (Set.singleton (Set.fromList [fromPositive "x", fromNegative "y"]))
        let expectedFormula = Or (Var "x") (Not (Var "y"))
        assertEqual "Convert single clause CNF to Formula" expectedFormula (toFormulaCNF cnf),

    -- Nouveaux tests
    TestLabel "Multiple Clauses CNF to Formula" $ TestCase $ do
        let cnf = CNF (Set.fromList [
                        Set.fromList [fromPositive "x", fromPositive "y"],
                        Set.fromList [fromNegative "z"]
                      ])
        let expectedFormula = And (Or (Var "x") (Var "y")) (Not (Var "z"))
        assertEqual "Convert multiple clauses CNF to Formula" expectedFormula (toFormulaCNF cnf),
        
    TestLabel "Mixed literals CNF to Formula" $ TestCase $ do
        let cnf = CNF (Set.fromList [
                    Set.fromList [fromPositive "x", fromNegative "y"],
                    Set.fromList [fromPositive "z", fromNegative "w"]
                  ])
        let expectedFormula = And (Or (Var "x") (Not (Var "y"))) (Or (Var "z") (Not (Var "w")))
        assertEqual "Convert mixed literals CNF to Formula" expectedFormula (toFormulaCNF cnf)
    ]


-- Test pour convertir une formule simple en CNF
testFromFormula :: Test
testFromFormula = TestList [
    -- Tests existants
    TestLabel "Simple Formula to CNF" $ TestCase $ do
        let formula = And (Var "x") (Not (Var "y"))
        let expectedCNF = CNF (Set.fromList [Set.fromList [fromPositive "x"], Set.fromList [fromNegative "y"]])
        assertEqual "Convert simple Formula to CNF" expectedCNF (fromFormula formula),

    -- Nouveaux tests ajoutés
    TestLabel "Complex Formula to CNF" $ TestCase $ do
        let formula = And (Var "x") (Or (Var "y") (Not (Var "z")))
        let expectedCNF = CNF (Set.fromList [
                                 Set.fromList [fromPositive "x", fromPositive "y"],
                                 Set.fromList [fromPositive "x", fromNegative "z"]
                               ])
        assertEqual "Convert complex Formula to CNF" expectedCNF (fromFormula formula),

    TestLabel "Implication Formula to CNF" $ TestCase $ do
        let formula = Imp (Var "x") (Var "y")
        let expectedCNF = CNF (Set.fromList [Set.fromList [fromNegative "x", fromPositive "y"]])
        assertEqual "Convert Implication Formula to CNF" expectedCNF (fromFormula formula),

    -- Ajout de nouveaux tests
    TestLabel "Nested Formulas to CNF" $ TestCase $ do
        let formula = Or (And (Var "a") (Var "b")) (Not (And (Var "c") (Var "d")))
        let expectedCNF = CNF (Set.fromList [
                                 Set.fromList [fromPositive "a", fromNegative "c"],
                                 Set.fromList [fromPositive "a", fromNegative "d"],
                                 Set.fromList [fromPositive "b", fromNegative "c"],
                                 Set.fromList [fromPositive "b", fromNegative "d"]
                               ])
        assertEqual "Convert nested formulas to CNF" expectedCNF (fromFormula formula),

    TestLabel "Disjunction of Implications to CNF" $ TestCase $ do
        let formula = Or (Imp (Var "a") (Var "b")) (Imp (Var "c") (Var "d"))
        let expectedCNF = CNF (Set.fromList [
                                 Set.fromList [fromNegative "a", fromPositive "b", fromNegative "c"],
                                 Set.fromList [fromNegative "a", fromPositive "b", fromPositive "d"]
                               ])
        assertEqual "Convert disjunction of implications to CNF" expectedCNF (fromFormula formula)
    ]




-- Test de l'algorithme de Robinson pour la résolution de clauses
testRobinson :: Test
testRobinson = TestCase $ do
  let clause1 = Set.fromList [fromPositive "A", fromNegative "B"]
      clause2 = Set.fromList [fromNegative "A", fromPositive "B"]
      clause3 = Set.fromList [fromPositive "C", fromPositive "D"]
      clause4 = Set.fromList [fromNegative "C", fromNegative "D"]
      cnf = CNF $ Set.fromList [clause1, clause2, clause3, clause4]

  let result = robinson cnf
  assertEqual "resolves complementary literals" result (CNF (Set.singleton Set.empty))

-- Test pour vérifier que Robinson laisse les clauses non résolvables inchangées
testRobinson2 :: Test
testRobinson2 = TestCase $ do
  let clause1 = Set.fromList [fromPositive "A", fromPositive "B"]
      clause2 = Set.fromList [fromNegative "C", fromNegative "D"]
      cnf = CNF $ Set.fromList [clause1, clause2]

  let result = robinson cnf
  assertEqual "leaves non-resolvable clauses unchanged" result (CNF $ Set.fromList [clause1, clause2])

-- Test pour vérifier la distribution de 'And' sur 'Or'
testDistribute :: Test
testDistribute = TestList [
    TestLabel "Distribute And over Or" $ TestCase $ do
        let formula = And (Var "x") (Or (Var "y") (Var "z"))
        let expectedFormula = Or (And (Var "x") (Var "y")) (And (Var "x") (Var "z"))
        assertEqual "Distribute And over Or" expectedFormula (distribute formula)
    ]

-- Fonction principale pour exécuter tous les tests
main :: IO ()
main = do
    runTestTT $ TestList [testSize, testToFormulaCNF, testFromFormula, testRobinson, testDistribute]
    return ()
