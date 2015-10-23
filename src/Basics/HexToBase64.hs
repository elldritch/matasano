module Basics.HexToBase64 (transcode16to64) where

import Util.Convert (decode16', encode64')

transcode16to64 :: String -> String
transcode16to64 = encode64' . decode16'
