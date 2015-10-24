module Basics.FixedXor (fixedXor) where

import Data.Bits (xor)
import Util.Convert (decode16', encode16', viewN, makeN)

fixedXor :: String -> String -> String
fixedXor a b = intsToHex $ zipWith xor (hexToInts a) (hexToInts b :: [Int])
  where hexToInts = viewN . decode16'
        intsToHex = encode16' . makeN
