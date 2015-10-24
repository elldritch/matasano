module Basics.AESInECB (aesEcbDecrypt) where

import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Maybe

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types
import Crypto.Error

import Util.Convert

aesEcbDecrypt :: String -> String -> String
aesEcbDecrypt key ciphertext = viewS plaintext
  where keyBytes = makeS key
        cipher = throwCryptoError $ cipherInit keyBytes :: AES128
        plaintext = ecbDecrypt cipher $ decode64' $ concat $ lines ciphertext
