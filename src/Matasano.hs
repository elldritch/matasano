module Matasano (solutions) where

import Basics.HexToBase64 (transcode16to64)
import Basics.FixedXor (fixedXor)
import Basics.SingleByteXorCipher (bestXor1Candidate)
import Basics.DetectSingleCharacterXor (bestXor1Candidate')
import Basics.RepeatingKeyXor (xorEncrypt)

args1 :: (String -> String) -> [String] -> String
args1 f = f . head

args2 :: (String -> String -> String) -> [String] -> String
args2 f (x:y:xs) = f x y

s1q1 = args1 transcode16to64
s1q2 = args2 fixedXor
s1q3 = args1 bestXor1Candidate
s1q4 = bestXor1Candidate'
s1q5 = args2 xorEncrypt

solutions :: [[[String] -> String]]
solutions = [
    [s1q1, s1q2, s1q3, s1q4, s1q5]
  ]
