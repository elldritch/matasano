module Matasano (solutions) where

import Basics.HexToBase64 (transcode16to64)
import Basics.FixedXor (fixedXor)
import Basics.SingleByteXorCipher (singleXorBreak)
import Basics.DetectSingleCharacterXor (singleXorBreakMany)
import Basics.RepeatingKeyXor (repeatingXorEncrypt)
import Basics.BreakRepeatingKeyXor (repeatingXorBreak)

args1 :: (String -> String) -> [String] -> String
args1 f = f . head

args2 :: (String -> String -> String) -> [String] -> String
args2 f (x:y:xs) = f x y

s1q1 = args1 transcode16to64
s1q2 = args2 fixedXor
s1q3 = args1 $ snd . singleXorBreak
s1q4 = snd . singleXorBreakMany
s1q5 = args2 repeatingXorEncrypt
s1q6 = args1 repeatingXorBreak

solutions :: [[[String] -> String]]
solutions = [
    [s1q1, s1q2, s1q3, s1q4, s1q5, s1q6]
  ]
