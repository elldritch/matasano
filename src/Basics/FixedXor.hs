module Basics.FixedXor (fixedXor) where

import Data.Bits (xor)
import Util.Convert (decode16, encode16)

fixedXor :: String -> String -> String
fixedXor a b = encode16 $ xor (decode16 a) (decode16 b :: Integer)
