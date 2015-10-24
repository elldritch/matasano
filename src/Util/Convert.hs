module Util.Convert (
  padLeading, pad0,
  decode16', encode16',
  decode64', encode64',
  viewS, viewN,
  makeS, makeN
) where

import Numeric (showIntAtBase, readInt,
                readHex, showHex)

import Data.ByteString as BS (ByteString, pack, unpack)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base64 as BS64

import Data.Char (intToDigit, digitToInt)
import Data.Maybe (fromMaybe)
import Data.List (elemIndex, find)
import Data.List.Split (chunksOf)

padLeading :: Char -> Int -> String -> String
padLeading c l s = _padLeading c (l - length s) s

_padLeading :: Char -> Int -> String -> String
_padLeading c l s = if l > 0 then c : _padLeading c (l - 1) s else s

pad0 :: Int -> String -> String
pad0 = padLeading '0'

decode16' :: String -> ByteString
decode16' s = BS.pack $ map (fst . head . readHex) $ chunksOf 2 s

encode16' :: ByteString -> String
encode16' b = concatMap (\w -> pad0 2 $ showHex w "") $ BS.unpack b

encode64' :: ByteString -> String
encode64' b = BS8.unpack $ BS64.encode b

decode64' :: String -> ByteString
decode64' s = case BS64.decode $ BS8.pack $ concat $ lines s of
  Left err -> error err
  Right result -> result

viewS :: ByteString -> String
viewS = BS8.unpack

viewN :: (Integral a) => ByteString -> [a]
viewN b = map fromIntegral $ BS.unpack b

makeS :: String -> ByteString
makeS = BS8.pack

makeN :: (Integral a) => [a] -> ByteString
makeN xs = BS.pack $ map fromIntegral xs
