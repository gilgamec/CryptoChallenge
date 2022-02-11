-- Test for Challenge 18.
-- Check that the mystery string decrypts correctly with the given key.

module Test18 ( tests ) where

import Bytes ( HasBytes(..), Bytes )
import Bytes.Base64 ( Base64, mkBase64 )
import AES ( decryptCTR )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B

import TestFramework

mysteryString :: Base64
mysteryString = fromJust $ mkBase64
  "L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ=="

decryptedString :: String
decryptedString = "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby "

givenKey :: String
givenKey = "YELLOW SUBMARINE"

givenNonce :: Bytes
givenNonce = B.replicate 16 0

tests =
  [ testGroup "Challenge 18"
    [ testCase "correctly decrypt given string" $
        decryptCTR givenKey givenNonce mysteryString @?=
          Just (toBytes decryptedString) ]
  ]
