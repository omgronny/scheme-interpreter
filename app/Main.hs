module Main (main) where

import Cli (parseInput)
import Parser

import Text.Parsec (parse)
import Text.Printf
import System.Console.Haskeline
import Data.Maybe (fromMaybe)
import Control.Monad.IO.Class (liftIO)


--------------------------------------------------------------------------------

main :: IO ()
main = do
  print $ "hello1"
  doMain

doMain = do
  print $ "> "
  input <- getLine
  let res = parse pForm "(source)" input
  case res of
    Left _ -> do
      print $ "Error!"
    Right form -> do
      print $ form
  doMain
