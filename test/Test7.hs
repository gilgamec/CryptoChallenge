-- Test for Challenge 7.
-- Check that the input file is correctly decrypted with the given key.

module Test7 ( tests ) where

import Bytes ( HasBytes(..) )
import Bytes.Base64 ( mkBase64 )
import AES ( decryptECB )

import qualified Data.ByteString as B

import TestFramework

-- Input data file
inputFileName = "data/07.txt"

-- Key
key = "YELLOW SUBMARINE"

-- First couple of lines of the plaintext
plain = "I'm back and I'm ringin' the bell \nA rockin' on the mike while the fly girls yell"

tests =
  [ testGroup "Challenge 7"
    [ testCase "correct decryption of input file" $ do
        Just bs <- mkBase64 . concat . lines <$> readFile inputFileName
        case decryptECB key bs of
          Nothing -> assertFailure "ECB decryption failed"
          Just ds -> B.take (length plain) ds @?= toBytes plain
    ]
  ]
