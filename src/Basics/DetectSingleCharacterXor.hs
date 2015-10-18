module Basics.DetectSingleCharacterXor (singleXorBreakMany) where

import Data.List (maximumBy)

import Util.Cipher (scoreOrd)
import Basics.SingleByteXorCipher (singleXorBreak)

singleXorBreakMany :: [String] -> String
singleXorBreakMany xs = maximumBy scoreOrd candidates
  where candidates = map singleXorBreak xs
