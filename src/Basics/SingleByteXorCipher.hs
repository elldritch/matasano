module Basics.SingleByteXorCipher (singleXorBreak) where

import Data.List (maximumBy)
import Data.List.Split (chunksOf)

import Util.Cipher (singleXorDecrypt', scoreOrd)
import Util.Convert (decode16', viewN)

singleXorBreak :: String -> String
singleXorBreak ciphertext = maximumBy scoreOrd candidates
  where keys = [0..255]
        candidates = [intsToChars $ singleXorDecrypt' k $ hexToInts ciphertext | k <- keys]
        hexToInts = viewN . decode16'
        intsToChars = map toEnum
