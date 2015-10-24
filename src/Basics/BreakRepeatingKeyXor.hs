module Basics.BreakRepeatingKeyXor (repeatingXorBreak) where

import Data.Bits (xor, popCount)
import Data.List (minimumBy, transpose)
import Data.List.Split (chunksOf)
import Data.Ord (comparing)

import Util.Convert (decode16, decode64, viewS, makeS)

import Basics.SingleByteXorCipher (singleXorBreak)
import Basics.RepeatingKeyXor (repeatingXorDecrypt)

repeatingXorBreak :: String -> String
repeatingXorBreak input = viewS $ decode16 $ repeatingXorDecrypt key ciphertext
  where ciphertext = base64ToChars input
        keySize = fst $ minimumBy (comparing snd) [(ks, blockDistance' ks ciphertext) | ks <- [4..40]]
        chunks = transpose $ chunksOf keySize ciphertext
        key = map (toEnum . fst . singleXorBreak . makeS) chunks
        base64ToChars = viewS . decode64

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

blockDistance' :: (Fractional a) => Int -> String -> a
blockDistance' keySize ciphertext = averageDistance / fromIntegral keySize
  where blocks = chunksOf keySize $ take (keySize * 4) ciphertext
        distances = tail $ zipWith hamming' (blocks ++ []) ([] : blocks)
        averageDistance = average $ map fromIntegral distances

hamming' :: String -> String -> Int
hamming' a b = sum $ map popCount diffs
  where xs = charToInts a
        ys = charToInts b
        diffs = zipWith xor xs ys
        charToInts = map fromEnum
