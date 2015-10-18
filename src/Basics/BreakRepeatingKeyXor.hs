module Basics.BreakRepeatingKeyXor (repeatingXorBreak) where

import Numeric
import Data.Char
import Data.List
import Data.List.Split
import Data.Ord

import Util.Convert (decode2, encode2,
                     decode16, encode16,
                     decode64Char,
                     pad0)
import Util.Cipher (singleXorDecrypt, score)

import Basics.RepeatingKeyXor

-- Utility functions
encodeBinary :: (Integral a, Show a) => a -> String
encodeBinary = pad0 8 . encode2

transcodeAsciiToBinary :: String -> String
transcodeAsciiToBinary = concatMap (encodeBinary . fromEnum)

hamming :: String -> String -> Int
hamming _ [] = 0
hamming [] _ = 0
hamming (x:xs) (y:ys)
  | x /= y    = 1 + rest
  | otherwise = rest
    where rest = hamming xs ys

hammingBinary :: String -> String -> Int
hammingBinary a b = hamming (transcodeAsciiToBinary a) (transcodeAsciiToBinary b)

average :: (Fractional a) => [a] -> a
average xs = sum xs / fromIntegral (length xs)

-- Breakin'
keySizes = [4..200]

blockNormDistance :: (Fractional a) => Int -> String -> a
blockNormDistance ks ciphertext = distance / fromIntegral ks
  where blocks = chunksOf ks $ take (ks * 4) ciphertext
        distances = tail $ zipWith hammingBinary (blocks ++ []) ([] : blocks)
        distance = average $ map fromIntegral distances

singleXorBreak :: String -> (Int, String)
singleXorBreak ciphertext = maximumBy (comparing (score . snd)) candidates
  where keys = [0..255]
        candidates = zip keys [singleXorDecrypt k $ map fromEnum ciphertext | k <- keys]

transcodeBinaryToAscii :: String -> String
transcodeBinaryToAscii s = map (toEnum . decode2) $ chunksOf 8 s

repeatingXorBreak :: String -> String
repeatingXorBreak ciphertext = show $ hexToAscii $ repeatingXorDecrypt key cipherbin
  where cipherbin = transcodeBinaryToAscii $ concatMap (pad0 6 . encode2) $ init $ map decode64Char $ concat $ lines ciphertext
        keySize = fst $ minimumBy (comparing snd) [(ks, blockNormDistance ks cipherbin) | ks <- keySizes]
        chunks = chunksOf keySize cipherbin
        transposed = transpose chunks
        keyComputed = map (toEnum . fst . singleXorBreak :: String -> Char) transposed
        keyActual = "Terminator X: Bring the noise"
        key = keyComputed
        hexToAscii s = map (toEnum . decode16 :: String -> Char) $ chunksOf 2 s
