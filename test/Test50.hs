-- Test for Challenge 50.
-- Checks that we can inject evil Javascript even though
-- the script is signed.

module Test50 ( tests ) where

import Bytes ( HasBytes(..), convBytes )
import Bytes.Hex ( mkHex )
import Hash ( MAC(..) )

import Challenge49 ( CBCMAC, mkCBCMAC, validateCBCMAC )
import Challenge50 ( fakeJavascript )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B

import TestFramework

-- The Javascript given in the Challenge description,
-- and its hash under the given key and IV.
script = "alert('MZA who was that?');\n"

key = "YELLOW SUBMARINE"

iv = B.replicate 16 0

scriptHash = toBytes . fromJust . mkHex $ "296b8d7cb78a243dda4d0a61d33bbdd1"

-- The evil Javascript we have to inject with the same hash.
evilScript = "alert('Ayo, the Wu is back!')"

tests =
  [ testGroup "Challenge 50"
    [ testCase "ensure the hash matches for the given script" $ do
        let mac = mkCBCMAC key iv script
        macHash mac @?= convBytes scriptHash
    , testCase "inject the evil code with the same hash" $ do
        let goodMAC = mkCBCMAC key iv script
            padded = fakeJavascript key iv scriptHash evilScript
            evilMAC = mkCBCMAC key iv padded
        macHash evilMAC @?= macHash goodMAC
    ]
  ]
