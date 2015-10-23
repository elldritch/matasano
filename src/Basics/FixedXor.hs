module Basics.FixedXor (fixedXor) where

import Data.Bits (xor)
import Util.Convert (decode16', encode16', viewN, makeN)

fixedXor :: String -> String -> String
fixedXor a b = encode16' $ makeN $ zipWith xor (hexToInts a) (hexToInts b)
  where hexToInts = viewN . decode16'
