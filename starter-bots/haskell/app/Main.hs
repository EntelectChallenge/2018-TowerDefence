module Main where

import Interpretor (repl)
import Bot (decide)
import System.Random

main :: IO ()
main = do
  gen <- getStdGen
  repl (decide gen)
