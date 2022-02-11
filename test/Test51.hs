-- Tests for Challenge 51.
-- Checks that we can break a session ID by looking at
-- differences in compressed length, for stream- and block-ciphered IDs.

module Test51 ( tests ) where

import Bytes ( convBytes )
import Random ( randomBytes )

import Challenge51 ( lengthOracleCTR, compressionAttackStream
                   , lengthOracleCBC, compressionAttackBlock )

import TestFramework

tests =
  [ testGroup "Challenge 51"
    [ repeatTestCase "break a stream-ciphered session ID" 10 $ do
        key <- randomBytes 16
        sessionID <- randomBytes 24
        let oracle = lengthOracleCTR key sessionID
        compressionAttackStream oracle @?= convBytes sessionID
    , repeatTestCase "break a block-ciphered session ID" 10 $ do
        key <- randomBytes 16
        sessionID <- randomBytes 24
        let oracle = lengthOracleCBC key sessionID
        compressionAttackBlock oracle @?= convBytes sessionID
    ]
  ]
