import Test.HUnit (Test (..), assertEqual, runTestTTAndExit)

import Parser
import Evaluator
import Types
import Text.Parsec (parse)

main :: IO ()
main = runTestTTAndExit tests

evaluateProgram :: Variables -> String -> (Variables, Maybe String)
evaluateProgram vars input = do
    case parse pForm "(source)" input of
        Left _ -> do
            (vars, Nothing)
        Right form -> do
            let (vars', ev) = eval vars form
            case ev of
                Nothing -> (vars', Nothing)
                Just evaluated -> (vars', Just $ show evaluated)

checkEquals :: Variables -> String -> String -> IO Variables
checkEquals vars input expected = do
    let (vars', evaluated) = evaluateProgram vars input
    assertEqual "" (Just expected) evaluated
    return vars'

checkNothing  :: Variables -> String -> IO Variables
checkNothing vars input = do
    let (vars', evaluated) = evaluateProgram vars input
    assertEqual "" Nothing evaluated
    return vars'

tests :: Test
tests =
  TestList
    [ TestLabel "linear: test simple number" test1,
        TestLabel "linear: test simple bool" test2,
        TestLabel "linear: test list numbers" test3,
        TestLabel "linear: test list order" test4,
        TestLabel "linear: test list bool func" test5,
        TestLabel "linear: test define" test6,
        TestLabel "linear: test laziness" test7,
        TestLabel "linear: test quote" test8
    ]

test1 :: Test
test1 =
  TestCase
    ( do
        checkEquals initVars "5" "5"

        checkEquals initVars "-5" "-5"

        checkEquals initVars "+5" "5"

        checkEquals initVars "(number? 5)" "#t"

        checkEquals initVars "(number? #t)" "#f"

        return ()
    )

test2 :: Test
test2 =
  TestCase
    ( do
        checkEquals initVars "#t" "#t"
        checkEquals initVars "#f" "#f"

        checkEquals initVars "(boolean? #t)" "#t"
        checkEquals initVars "(boolean? #f)" "#t"

        checkEquals initVars "(boolean? 5)" "#f"
        checkEquals initVars "(boolean? \"lalala\")" "#f"
        checkEquals initVars "(boolean? \"#t\")" "#f"

        return ()
    )

test3 :: Test
test3 =
  TestCase
    ( do
        checkEquals initVars "(max 2 1)" "2"
        checkEquals initVars "(max 1 2 5 3 2 1 4)" "5"
        
        checkEquals initVars "(min 38 2 7 8 3 0 7)" "0"

        checkEquals initVars "(+ 1 2)" "3"
        checkEquals initVars "(+ 1 2 3 4 5)" "15"
        
        checkEquals initVars "(* 1 2 3)" "6"

        checkEquals initVars "(+ 1 (+ 3 4 5))" "13"
        
        checkEquals initVars "(/ 4 2)" "2"
        checkEquals initVars "(/ 4 2 2)" "1"

        checkEquals initVars "(abs 10)" "10"
        checkEquals initVars "(abs -10)" "10"

        return ()
    )

test4 :: Test
test4 =
  TestCase
    ( do
        checkEquals initVars "(> 5 4 3 2 1)" "#t"

        checkEquals initVars "(> 5 4 2 3 1)" "#f"

        checkEquals initVars "(>= 5 4 3 3 1)" "#t"

        checkEquals initVars "(< 5 4 3 3 1)" "#f"

        return ()
    )

test5 :: Test
test5 =
  TestCase
    ( do
        checkEquals initVars "(not #t)" "#f"
        checkEquals initVars "(not #f)" "#t"

        checkEquals initVars "(not 1)" "#f"
        checkEquals initVars "(not 0)" "#f"

        checkEquals initVars "(and)" "#t"
        checkEquals initVars "(and (= 2 2) (> 2 1))" "#t"
        checkEquals initVars "(and (= 2 2) (< 2 1))" "#f"
        checkEquals initVars "(and 1 2 3)" "3"

        checkEquals initVars "(or)" "#t"
        checkEquals initVars "(or (not (= 2 2)) (> 2 1))" "#t"
        checkEquals initVars "(or #f (< 2 1))" "#f"
        checkEquals initVars "(or #f 1)" "1"

        return ()
    )

test6 :: Test
test6 =
  TestCase
    ( do
        let vars = initVars

        vars <- checkEquals vars "(define x 3)" "3"
        vars <- checkEquals vars "x" "3"

        vars <- checkEquals vars "(define x (+ 1 3))" "4"
        vars <- checkEquals vars "x" "4"

        vars <- checkEquals vars "(set! x (+ 2 5))" "7"
        vars <- checkEquals vars "x" "7"

        return ()
    )

test7 :: Test
test7 =
  TestCase
    ( do
        let vars = initVars

        vars <- checkEquals vars "(define x 1)" "1"
        vars <- checkEquals vars "x" "1"

        vars <- checkEquals vars "(and #f (set! x 2))" "#f"
        vars <- checkEquals vars "x" "1"

        return ()
    )

test8 :: Test
test8 =
  TestCase
    ( do
        checkEquals initVars "'(1 2)" "(1 2)"

        checkEquals initVars "'(f g)" "(\"f\" \"g\")"
        return ()
    )
