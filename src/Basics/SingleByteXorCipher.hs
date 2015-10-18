module Basics.SingleByteXorCipher (bestXor1Candidate) where

import Data.List (maximumBy)

import Util.Cipher (xorDecrypt, scoreOrd)
import Util.Convert (decodeAscii)

bestXor1Candidate :: String -> String
bestXor1Candidate ciphertext = maximumBy scoreOrd candidates
  where keys = [0..255]
        candidates = [xorDecrypt k $ decodeAscii ciphertext | k <- keys]
