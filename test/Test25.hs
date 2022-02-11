-- Tests for Challenge 25.
-- Check that we can edit text locally through CTR,
-- then check that this means that we can trivially break same.

module Test25 ( tests ) where

import Bytes ( HasBytes(..) )
import Random ( randomBytes, randomBytesR )
import AES ( encryptCTR, decryptCTR )

import Challenge25 ( editCTR, breakEditCTR )

import TestFramework

tests =
  [ testGroup "Challenge 25"
    [ testCase "edit under a CTR cipher" $ do
        key <- randomBytes 16
        nonce <- randomBytes 8
        let plaintext = "My hovercraft is full of eels!"
            Just ct = encryptCTR key nonce plaintext
            editFunc = editCTR key nonce
            ct' = editFunc 25 "feet" ct
        decryptCTR key nonce ct' @?=
          Just (toBytes "My hovercraft is full of feet!")
    , testCase "trivially break a random-access CTR" $ do
        key <- randomBytes 16
        nonce <- randomBytes 8
        text <- randomBytesR (40,80)
        let Just ct = encryptCTR key nonce text
            editFunc = editCTR key nonce
        breakEditCTR editFunc ct @?= text ]
  ]
