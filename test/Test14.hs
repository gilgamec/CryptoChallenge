-- Test for Challenge 14.
-- Check that we can correctly decrypt the mystery text.

module Test14 ( tests ) where

import Challenge14 ( decryptInfixECB )

import Bytes ( HasBytes(..), Bytes )
import Bytes.Base64 ( Base64, mkBase64 )
import Random ( randomBytes, randomBytesR )
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

encryptOracle :: (HasBytes key, HasBytes prefix, HasBytes text)
              => key -> prefix -> text -> Bytes
encryptOracle key prefix text =
  fromJust $ encryptECB key $ padPKCS7 16 $
  toBytes prefix <> toBytes text <> toBytes mysteryText

tests =
  [ testGroup "Challenge 14"
    [ testCase "correctly decrypt mystery text" $ do
      key <- randomBytes 16
      prefix <- randomBytesR (20,40)
      let decrypted = decryptInfixECB (encryptOracle key prefix)
      decrypted @?= toBytes mysteryText ]
  ]
