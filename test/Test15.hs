-- Tests for Challenge 15.
-- Checks that PKCS#7 validation works as expected.

module Test15 ( tests ) where

import Bytes ( HasBytes(..), Bytes )
import Padding.PKCS7 ( validatePKCS7 )

import Data.Maybe ( isNothing )

import qualified Data.ByteString as B

import TestFramework

testStr :: Bytes
testStr = toBytes "ICE ICE BABY"

tests =
  [ testGroup "Challenge 15"
    [ testCase "test case 1 valid" $
      validatePKCS7 (testStr <> B.replicate 4 4) @?= Just testStr
  ,   testCase "test case 2 invalid" $
      isNothing (validatePKCS7 (testStr <> B.replicate 4 5)) @?
        "validated an invalid test case"
  ,   testCase "test case 3 invalid" $
      validatePKCS7 (testStr <> toBytes "\x01\x02\x03\x04") @?= Nothing
  ,   testCase "test case 4 valid-ish" $
      validatePKCS7 (testStr <> toBytes "\x04\x03\x02\x01") @?=
        Just (testStr <> toBytes "\x04\x03\x02") ]
  ]
