module Basics.SingleByteXorCipher (singleXorBreak) where

import Data.List (maximumBy)
import Data.List.Split (chunksOf)

import Util.Cipher (singleXorDecrypt', scoreOrd)
import Util.Convert (decode16', viewN)

singleXorBreak :: String -> String
singleXorBreak ciphertext = maximumBy scoreOrd candidates
  where keys = [0..255]
        hexToInts = viewN . decode16'
        candidates = [map toEnum $ singleXorDecrypt' k $ hexToInts ciphertext | k <- keys]
