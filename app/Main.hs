module Main (main) where

import Cli (parseInput)
import Parser
import Evaluator
import Types

import Text.Parsec (parse)
import Text.Printf
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  doMain initVars

doMain vars = do
  print $ "> "
  input <- getLine
  let res = parse pForm "(source)" input
  case res of
    Left _ -> do
      print $ "Error!"
      doMain vars
    Right form -> do
      let (vars', value) = eval vars form
      print $ value
      doMain vars'
