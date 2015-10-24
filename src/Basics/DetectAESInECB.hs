module Basics.DetectAESInECB (detectAesEcb) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (take, drop, length)
import Data.List (nub, minimumBy)
import Data.Ord (comparing)

import Util.Convert (viewS)

detectAesEcb :: [ByteString] -> String
detectAesEcb ciphertext = viewS $ fst $ minimumBy (comparing snd) candidates
  where candidates = zip ciphertext $ map (length . nub . blocks) ciphertext
        blocks bs | BS.length bs > 3 = BS.take 2 bs : blocks (BS.drop 2 bs)
                  | otherwise = [bs]
