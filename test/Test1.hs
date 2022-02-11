-- Test for Challenge 1.
-- Checks that the given hex and Base64-encoded strings
-- are actually equivalent.

module Test1 ( tests ) where

import Bytes ( HasBytes(..), convBytes )
import Bytes.Hex ( mkHex )
import Bytes.Base64 ( mkBase64 )

import Data.Maybe ( isJust )

import TestFramework

-- The supposedly-equivalent hex-encoded and Base64-encoded strings.
hexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
base64String = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"

tests =
  [ testGroup "Challenge 1" $
    [ testCase "valid hex string" $
        isJust (mkHex hexString) @? "invalid hex string"
    , testCase "valid Base64 string" $
        isJust (mkBase64 base64String) @? "invalid base64 string" ]
    ++ let Just hex = mkHex hexString
           Just b64 = mkBase64 base64String
       in [ testCase "compare equality in Bytes" $ toBytes hex @?= toBytes b64
          , testCase "compare equality in Hex" $ hex @?= convBytes b64
          , testCase "compare equality in Base64" $ convBytes hex @?= b64 ]
  ]
