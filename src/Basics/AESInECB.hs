module Basics.AESInECB (aesEcbDecrypt) where

import Crypto.Cipher.AES (AES128)
import Crypto.Cipher.Types (cipherInit, ecbDecrypt)
import Crypto.Error (throwCryptoError)

import Util.Convert (decode64, makeS, viewS)

aesEcbDecrypt :: String -> String -> String
aesEcbDecrypt key ciphertext = viewS plaintext
  where keyBytes = makeS key
        cipher = throwCryptoError $ cipherInit keyBytes :: AES128
        plaintext = ecbDecrypt cipher $ decode64 $ concat $ lines ciphertext
