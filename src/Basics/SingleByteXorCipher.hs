module Basics.SingleByteXorCipher (singleXorBreak) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (foldr)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Word (Word8)

import Util.Cipher (singleXorDecrypt', score)
import Util.Convert (decode16', viewN)

singleXorBreak :: ByteString -> (Int, String)
singleXorBreak ciphertext = maximumBy (comparing (score . snd)) candidates
  where keys = [0..255] :: [Word8]
        candidates = zip (map fromIntegral keys) [bytesToChars $ singleXorDecrypt' k ciphertext | k <- keys]
        hexToInts = viewN . decode16'
        bytesToChars = BS.foldr (\w s -> (toEnum $ fromIntegral w :: Char) : s) ""
