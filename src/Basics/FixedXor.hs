module Basics.FixedXor (fixedXor) where

import Data.Bits (xor)
import qualified Data.ByteString as BS (zipWith)

import Util.Convert (decode16', encode16', makeN)

fixedXor :: String -> String -> String
fixedXor a b = intsToHex $ BS.zipWith xor (decode16' a) (decode16' b)
  where intsToHex = encode16' . makeN
