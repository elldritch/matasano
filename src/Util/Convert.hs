module Util.Convert (
  decodeAscii,
  decode16, encode16,
  encode64
) where

import Numeric (showIntAtBase,
                readHex, showHex)
import Data.List.Split (chunksOf)

decode16 :: (Integral a) => String -> a
decode16 s = fst $ head $ readHex s

encode16 :: (Integral a, Show a) => a -> String
encode16 s = showHex s ""

decodeAscii :: String -> [Int]
decodeAscii s = map decode16 $ chunksOf 2 s

base64Table = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/"

encode64Char :: Int -> Char
encode64Char n = base64Table !! n

encode64 :: (Integral a, Show a) => a -> String
encode64 n = showIntAtBase 64 encode64Char n ""
