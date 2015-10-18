module Basics.DetectSingleCharacterXor (bestXor1Candidate') where

import Data.List (maximumBy)

import Util.Cipher (scoreOrd)
import Basics.SingleByteXorCipher (bestXor1Candidate)

bestXor1Candidate' :: [String] -> String
bestXor1Candidate' xs = maximumBy scoreOrd candidates
  where candidates = map bestXor1Candidate xs
