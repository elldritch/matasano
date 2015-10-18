module Main where

import System.Environment (getArgs)
import Matasano (solutions)

main :: IO ()
main = do
  let q = head solutions !! 4
  args <- getArgs
  putStrLn $ q args
