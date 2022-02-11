-- Tests for Challenge 10.
-- Checks that the input file decrypts correctly with the given key.

module Test10 ( tests ) where

import Bytes ( HasBytes(..) )
import Bytes.Base64 ( mkBase64 )
import AES ( encryptECB, decryptECB, decryptCBC )

import qualified Data.ByteString as B

import TestFramework

-- Test text
text = "sample text of exactly 32 bytes."

-- Input data file
inputFileName = "data/10.txt"

-- Key and IV
key = "YELLOW SUBMARINE"
iv = B.replicate 16 0

-- First couple of lines of the plaintext
plain = "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell"

tests =
  [ testGroup "Challenge 10"
    [ testCase "AES encryption works" $
        (encryptECB key text >>= decryptECB key) @?= Just (toBytes text)
    , testCase "correct decryption of file" $ do
        Just bs <- mkBase64 . concat . lines <$> readFile inputFileName
        (B.take (length plain) <$> decryptCBC key iv bs) @?= Just (toBytes plain) ]
  ]
