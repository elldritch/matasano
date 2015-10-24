module Basics.DetectSingleCharacterXor (singleXorBreakMany) where

import Data.ByteString (ByteString)
import Data.List (maximumBy)
import Data.Ord (comparing)

import Util.Cipher (score)
import Basics.SingleByteXorCipher (singleXorBreak)

singleXorBreakMany :: [ByteString] -> (Int, String)
singleXorBreakMany xs = maximumBy (comparing (score . snd)) candidates
  where candidates = map singleXorBreak xs
