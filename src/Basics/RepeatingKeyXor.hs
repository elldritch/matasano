module Basics.RepeatingKeyXor (repeatingXorEncrypt, repeatingXorDecrypt) where

import Data.Bits (xor)
import Data.List.Split (chunksOf)

import Util.Convert (encode16, pad0)

repeatingXorEncrypt :: String -> String -> String
repeatingXorEncrypt key plaintext = concat ciphertext
  where chunks = chunksOf (length key) $ map fromEnum plaintext
        keyChunk = map fromEnum key
        encrypted = map (zipWith xor keyChunk) chunks
        ciphertext = map (pad0 2 . encode16) $ concat encrypted

repeatingXorDecrypt = repeatingXorEncrypt
