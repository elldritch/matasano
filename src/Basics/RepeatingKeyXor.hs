module Basics.RepeatingKeyXor (repeatingXorEncrypt, repeatingXorDecrypt) where

import Data.Bits (xor)
import Data.List.Split (chunksOf)

import Util.Convert (encode16, makeN, makeS, viewN)

repeatingXorEncrypt :: String -> String -> String
repeatingXorEncrypt key plaintext = intsToHex $ makeN $ concat encrypted
  where plainChunks = chunksOf (length key) $ charsToInts plaintext
        keyChunk = charsToInts key
        encrypted = map (zipWith xor keyChunk) plainChunks
        charsToInts cs = viewN $ makeS cs :: [Int]
        intsToHex = encode16

repeatingXorDecrypt = repeatingXorEncrypt
