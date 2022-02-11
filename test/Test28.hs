-- Tests for Challenge 28.
-- Checks that MACs can be validated and not (easily) forged.

module Test28 ( tests ) where

import Random ( randomBytes, randomBytesR )
import Hash ( MAC(..), mkSHA1MAC, validateSHA1MAC )

import TestFramework

tests =
  [ testGroup "Challenge 28"
    [ repeatTestCase "MAC of random data is valid" 10 $ do
        key <- randomBytes 16
        message <- randomBytesR (40,60)
        let mac = mkSHA1MAC key message
        validateSHA1MAC key mac @? "MAC was not valid"
    , testCase "MAC cannot (easily) be forged" $ do
        key <- randomBytes 16
        message <- randomBytesR (40,60)
        let mac = mkSHA1MAC key message
            mac' = MAC{ macMessage = "forged message"
                      , macHash = macHash mac }
        not (validateSHA1MAC key mac') @? "MAC was forged too easily" ]
  ]
