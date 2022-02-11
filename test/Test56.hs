-- Test for Challenge 56.
-- Checks that we can find an RC4 secret using single-byte biases.

module Test56 ( tests ) where

import Bytes ( convBytes )
import Bytes.Base64 ( Base64, mkBase64 )

import Challenge56 ( findAllBytes, rc4Oracle, freqTables )

import Data.Maybe ( fromJust )

import TestFramework

-- The secret given in the Challenge directions
secret :: Base64
secret = fromJust . mkBase64 $ "QkUgU1VSRSBUTyBEUklOSyBZT1VSIE9WQUxUSU5F"

tests =
  [ testGroup "Challenge 56"
    [ testCase "decrypt RC4-encrypted secret" $ do
        bs <- findAllBytes (rc4Oracle secret) freqTables
        bs @?= (convBytes secret)
    ]
  ]
