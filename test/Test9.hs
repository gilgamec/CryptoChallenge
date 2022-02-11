-- Tests for Challenge 9.
-- Check that padding is applied correctly in the two given instances.

module Test9 ( tests ) where

import Bytes ( HasBytes(..) )
import Padding.PKCS7 ( padPKCS7 )

import qualified Data.ByteString as B

import TestFramework

-- Block
block = "YELLOW SUBMARINE"

-- Padded to 20-byte block size
blockPadded20 = "YELLOW SUBMARINE\x04\x04\x04\x04"

-- Padded to 16-byte block size
blockPadded16 = "YELLOW SUBMARINE" ++ replicate 16 '\x10'

tests =
  [ testGroup "Challenge 9"
    [ testCase "correct padding to 16-byte blocks" $
        padPKCS7 16 block @?= toBytes blockPadded16
    , testCase "correct padding to 20-byte blocks" $
        padPKCS7 20 block @?= toBytes blockPadded20 ]
  ]
