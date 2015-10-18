module Basics.SingleByteXorCipher (singleXorBreak) where

import Data.List (maximumBy)
import Data.List.Split (chunksOf)

import Util.Cipher (singleXorDecrypt, scoreOrd)
import Util.Convert (decode16)

decodeAscii s = map decode16 $ chunksOf 2 s

singleXorBreak :: String -> String
singleXorBreak ciphertext = maximumBy scoreOrd candidates
  where keys = [0..255]
        candidates = [singleXorDecrypt k $ decodeAscii ciphertext | k <- keys]
