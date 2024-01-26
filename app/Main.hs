module Main (main) where

import Evaluator
import Parser
import System.Console.Haskeline
import Text.Parsec (parse)
import Text.Printf
import Types
import Control.Monad.IO.Class


--------------------------------------------------------------------------------

main :: IO ()
main = do
  runInputT defaultSettings (doMain initVars)

doMain :: [Variables] -> InputT IO ()
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
          (vars', value) <- liftIO $ eval vars form
          outputStrLn $ printf "%s" (show value)
          doMain vars'
