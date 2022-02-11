-- Test for Challenge 54.
-- Checks that we can create a message with a given hash
-- by precomputing a tree of colliding blocks.

module Test54 ( tests ) where

import Bytes.Integral ( littleEndian )
import Hash.MerkleDamgard ( mdHashOne, mdHash, mdIV )

import Challenge54 ( buildCollisionTree, finalHash, glueMessage )

import TestFramework

tests =
  [ testGroup "Challenge 54"
    [ testCase "fake a prediction under a 3-byte MD hash" $ do
        let hashLen = 3
            hash = mdHash hashLen
            hashOne = mdHashOne hashLen
            iv = mdIV hashLen
        -- Build a big tree of colliding hashes.
        let k = 8
            leafHashes = map (littleEndian hashLen) [1..2^k :: Int]
            ct = buildCollisionTree hashOne leafHashes
            msgLength = 2
            msgHash = finalHash hashOne ct msgLength
        -- All of this is done ahead of time.
        -- Now we wait....
        -- ...
        -- (some time later)
        -- The regular season is over!
        let message = take (msgLength * 16) $
                      "The Jays will win the pennant!" ++ repeat ' '
            finalMessage = glueMessage hashOne iv ct message
        hash finalMessage @?= msgHash
    ]
  ]
