module Util.Convert (
  padLeading, pad0,
  decode2, encode2,
  decode16, encode16,
  decode16', encode16',
  decode64, encode64,
  encode64',
  decode64Char, encode64Char
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

decode2 :: (Integral a) => String -> a
decode2 = fst . head . readInt 2 (\c -> c == '0' || c == '1') digitToInt

encode2 :: (Integral a, Show a) => a -> String
encode2 n = showIntAtBase 2 intToDigit n ""

decode16 :: (Integral a) => String -> a
decode16 = fst . head . readHex

decode16' :: String -> ByteString
decode16' s = BS.pack $ map (fst . head . readHex) $ chunksOf 2 s

encode16 :: (Integral a, Show a) => a -> String
encode16 n = showHex n ""

encode16' :: ByteString -> String
encode16' b = concatMap (`showHex` "") $ BS.unpack b

base64Table = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/"

encode64Char :: Int -> Char
encode64Char n = base64Table !! n

encode64 :: (Integral a, Show a) => a -> String
encode64 n = showIntAtBase 64 encode64Char n ""

encode64' :: ByteString -> String
encode64' b = BS8.unpack $ BS64.encode b

decode64Char :: Char -> Int
decode64Char c = fromMaybe (-1) $ elemIndex c base64Table

decode64 :: (Integral a) => String -> a
decode64 = fst . head . readInt 64 (\c ->
    case find (== c) base64Table of
      Just x -> True
      Nothing -> False)
  decode64Char
