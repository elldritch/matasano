module Main where

import System.Environment (getArgs)
import Matasano (solutions)

main :: IO ()
main = do
  let q = head solutions !! 5
  args <- getArgs
  file <- readFile "test/1-6-data"
  putStrLn $ q [file]
