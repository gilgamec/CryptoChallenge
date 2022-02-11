-- Tests for Challenge 55.
-- First checks that we implemented MD4 hashing correctly.
-- Then checks that we can generate an MD4 collision.

module Test55 ( tests ) where

import Bytes ( HasBytes(..) )
import Hash ( md4Hash )
import Random ( randomBytes )

import Challenge55 ( myMD4Hash, collideMD4 )

import TestFramework

-- test message
testMessage :: String
testMessage = "This is a test message, which should verify whether "++
              "MD4 has been correctly implemented. If the MD4 implementation "++
              "given here is correct, the hash it produces from this message "++
              "should be identical to the hash produced by cryptonite's "++
              "(presumably correct) implementation."

tests =
  [ testGroup "Challenge 55"
    [ testCase "implemented md4 correctly" $
        (toBytes $ myMD4Hash testMessage) @?= (toBytes $ md4Hash testMessage)
    , testCase "generate md4 collision" $ let
        go = do
          block <- randomBytes 64
          case collideMD4 block of
            Nothing -> go
            Just (m0,m1) -> do
              md4Hash m0 @?= md4Hash m1
              not (m0 == m1) @? "colliding hashes are identical"
       in go
    ]
  ]
