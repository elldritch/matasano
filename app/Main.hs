module Main where

import System.Environment (getArgs)
import Matasano (solutions)

main :: IO ()
main = do
  let q = head solutions !! 6
  args <- getArgs
  file <- readFile "test/1-7-data"
  putStrLn $ q ["YELLOW SUBMARINE", file]
