module Basics.RepeatingKeyXor (xorEncrypt) where

import Data.Bits (xor)
import Data.List.Split (chunksOf)

import Util.Convert (encode16)

encode16' n = if length unpadded < 2 then "0" ++ unpadded else unpadded
  where unpadded = encode16 n

xorEncrypt :: String -> String -> String
xorEncrypt key plaintext = concat ciphertext
  where chunks = chunksOf (length key) $ map fromEnum plaintext
        keyChunk = map fromEnum key
        encrypted = map (zipWith xor keyChunk) chunks
        ciphertext = map encode16' $ concat encrypted
