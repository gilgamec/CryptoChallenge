-- Test for Challenge 12.
-- Correctly decrypt the mystery text from an oracle.

module Test12 ( tests ) where

import Challenge12 ( decryptPrefixECB )

import Bytes ( HasBytes(..), Bytes )
import Bytes.Base64 ( Base64, mkBase64 )
import Random ( randomBytes )
import AES ( encryptECB )
import Padding.PKCS7 ( padPKCS7 )

import Data.Maybe ( fromJust )

import TestFramework

mysteryText :: Base64
mysteryText = fromJust (mkBase64 mysteryB64)
 where
  mysteryB64 = concat
    [ "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg"
    , "aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq"
    , "dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg"
    , "YnkK" ]

encryptOracle :: (HasBytes key, HasBytes text) => key -> text -> Bytes
encryptOracle key text = fromJust $ encryptECB key $ padPKCS7 16 $
                         toBytes text <> toBytes mysteryText

tests =
  [ testGroup "Challenge 12"
    [ testCase "correctly decrypt mystery text" $ do
      key <- randomBytes 16
      let decrypted = decryptPrefixECB (encryptOracle key)
      decrypted @?= toBytes mysteryText ]
  ]
