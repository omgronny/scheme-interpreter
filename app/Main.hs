module Main (main) where

import Evaluator
import Parser
import System.Console.Haskeline
import Text.Parsec (parse)
import Text.Printf
import Types

--------------------------------------------------------------------------------

main :: IO ()
main = do
  runInputT defaultSettings (doMain initVars)

doMain :: Variables -> InputT IO ()
doMain vars = do
  input <- getInputLine "> "
  case input of
    Nothing -> return ()
    Just inp -> do
      let res = parse pForm "(source)" inp
      case res of
        Left _ -> do
          outputStrLn $ printf "Error!"
          doMain vars
        Right form -> do
          let (vars', value) = eval vars form
          case value of
            Nothing -> outputStrLn $ printf "Syntax error"
            Just val -> outputStrLn $ printf "%s" (show val)
          doMain vars'
