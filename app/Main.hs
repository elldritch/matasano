module Main where

import System.Environment (getArgs)
import Matasano (solutions)

main :: IO ()
main = do
  let q = head solutions !! 7
  args <- getArgs
  file <- readFile "test/1-8-data"
  putStrLn $ q $ lines file
