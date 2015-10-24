module Basics.BreakRepeatingKeyXor (repeatingXorBreak) where

import Numeric
import Data.Bits
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord

import Util.Convert
import Util.Cipher

import Debug.Trace

-- import Basics.SingleByteXorCipher
import Basics.RepeatingKeyXor

repeatingXorBreak :: String -> String
repeatingXorBreak input = viewS $ decode16' $ repeatingXorDecrypt key ciphertext
  where ciphertext = base64ToChars input
        keySize = fst $ minimumBy (comparing snd) [(ks, blockDistance' ks ciphertext) | ks <- [4..40]]
        chunks = transpose $ chunksOf keySize ciphertext
        key = map (toEnum . fst . singleXorBreak :: String -> Char) chunks
        base64ToChars = viewS . decode64'

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

blockDistance' :: (Fractional a) => Int -> String -> a
blockDistance' keySize ciphertext = averageDistance / fromIntegral keySize
  where blocks = chunksOf keySize $ take (keySize * 4) ciphertext
        distances = tail $ zipWith hamming' (blocks ++ []) ([] : blocks)
        averageDistance = average $ map fromIntegral distances

hamming' :: String -> String -> Int
hamming' a b = sum $ map popCount diffs
  where as = map fromEnum a
        bs = map fromEnum b
        diffs = zipWith xor as bs

singleXorBreak :: String -> (Int, String)
singleXorBreak ciphertext = maximumBy (comparing (score . snd)) candidates
  where keys = [0..255]
        candidates = zip keys [map toEnum $ singleXorDecrypt' k $ map fromEnum ciphertext | k <- keys]
        -- candidates = traceShow ciphertext $ zip keys [map toEnum $ singleXorDecrypt' k $ viewN $ decode16' ciphertext | k <- keys]
