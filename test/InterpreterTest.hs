import Evaluator
import Parser
import Test.HUnit (Test (..), assertEqual, runTestTTAndExit)
import Text.Parsec (parse)
import Types

main :: IO ()
main = runTestTTAndExit tests

evaluateProgram :: [Variables] -> String -> IO ([Variables], Maybe String)
evaluateProgram vars input = do
  case parse pForm "(source)" input of
    Left _ -> do
      return $ (vars, Nothing)
    Right form -> do
      (vars', ev) <- eval vars form
      return $ (vars', Just $ show ev)

checkEquals :: [Variables] -> String -> String -> IO [Variables]
checkEquals vars input expected = do
  (vars', evaluated) <- evaluateProgram vars input
  assertEqual "" (Just expected) evaluated
  return vars'

tests :: Test
tests =
  TestList
    [ TestLabel "test simple number" test1,
      TestLabel "test simple bool" test2,
      TestLabel "test list numbers" test3,
      TestLabel "test list order" test4,
      TestLabel "test list bool func" test5,
      TestLabel "test define" test6,
      TestLabel "test laziness" test7,
      TestLabel "test quote" test8,
      TestLabel "test lambda" test9,
      TestLabel "test car-cdr" test10,
      TestLabel "test control flow" test11,
      TestLabel "test recursive" test12
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

test9 :: Test
test9 =
  TestCase
    ( do
        let vars = initVars
        vars <- checkEquals vars "(define test (lambda (x) (+ x 9)))" "lambda"
        vars <- checkEquals vars "(test 1)" "10"
        vars <- checkEquals vars "(test -1)" "8"
        vars <- checkEquals vars "(test 42)" "51"

        let vars = initVars
        vars <- checkEquals vars "(define test (lambda (x) (set! x (* x 2)) (+ 1 x)))" "lambda"
        vars <- checkEquals vars "(test 20)" "41"

        let vars = initVars
        vars <- checkEquals vars "(define add (lambda (x y) (+ x y 1)))" "lambda"
        vars <- checkEquals vars "(add -10 11)" "2"

        vars <- checkEquals vars "(define zero (lambda () 0))" "lambda"
        vars <- checkEquals vars "(zero)" "0"

        return ()
    )

test10 :: Test
test10 =
  TestCase
    ( do
        let vars = initVars

        vars <- checkEquals vars "(define x '(1 2))" "(1 2)"
        vars <- checkEquals vars "x" "(1 2)"

        vars <- checkEquals vars "(set-car! x 5)" "5"
        vars <- checkEquals vars "(car x)" "5"

        vars <- checkEquals vars "(set-cdr! x 6)" "6"
        vars <- checkEquals vars "(cdr x)" "6"

        vars <- checkEquals vars "x" "(5 6)"

        return ()
    )

test11 :: Test
test11 =
  TestCase
    ( do
        let vars = initVars

        vars <- checkEquals vars "(if #t 0 1)" "0"
        vars <- checkEquals vars "(if #f 0 1)" "1"

        vars <- checkEquals vars "(if (= 2 2) (+ 1 10) 42)" "11"
        vars <- checkEquals vars "(if (= 2 3) (+ 1 10) 42)" "42"

        vars <- checkEquals vars "(define x 1)" "1"
        vars <- checkEquals vars "(if #f (set! x 2) 42)" "42"
        vars <- checkEquals vars "x" "1"

        vars <- checkEquals vars "(if #t (set! x 4) (set! x 3))" "4"
        vars <- checkEquals vars "x" "4"

        return ()
    )

test12 :: Test
test12 =
  TestCase
    ( do
        let vars = initVars

        vars <- checkEquals vars "(define slow-add (lambda (x y) (if (= x 0) y (slow-add (- x 1) (+ y 1)))))" "lambda"

        vars <- checkEquals vars "(slow-add 0 3)" "3"
        vars <- checkEquals vars "(slow-add 3 3)" "6"
        vars <- checkEquals vars "(slow-add 100 100)" "200"

        return ()
    )
