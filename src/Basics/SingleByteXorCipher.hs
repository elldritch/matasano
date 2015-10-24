module Basics.SingleByteXorCipher (singleXorBreak) where

import Data.List (maximumBy)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

import Util.Cipher (singleXorDecrypt', score)
import Util.Convert (decode16', viewN)

singleXorBreak :: String -> (Int, String)
singleXorBreak ciphertext = maximumBy (comparing (score . snd)) candidates
  where keys = [0..255]
        candidates = zip keys [intsToChars $ singleXorDecrypt' k $ hexToInts ciphertext | k <- keys]
        hexToInts = viewN . decode16'
        intsToChars = map toEnum
