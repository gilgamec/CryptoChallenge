-- Test for Challenge 53.
-- Check that we can create a collision with expandable messages.

module Test53 ( tests ) where

import Hash.MerkleDamgard ( mdHashOne, mdHash, mdIV )
import Random ( randomBytes )

import Challenge53 ( collideLong )

import TestFramework

tests =
  [ testGroup "Challenge 53"
    [ testCase "create a collision under a 3-byte MD hash" $ do
        -- A message of 256 blocks = 4kbytes
        longMessage <- randomBytes (2^8 * 16)
        let hashLen = 3
            hash = mdHash hashLen
            forgery = collideLong (mdHashOne hashLen) (mdIV hashLen) longMessage
        hash longMessage @?= hash forgery
        not (longMessage == forgery) @? "forgery identical to original message"
    ]
  ]
