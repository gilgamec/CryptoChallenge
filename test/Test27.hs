-- Test for Challenge 27.
-- Check that we can indeed get the key if it's used as the IV.

module Test27 ( tests ) where

import Random ( randomBytes )

import Challenge27 ( weakCBC, validateWeakCBC, breakWeakCBC )

import Data.Maybe ( fromJust )

import TestFramework

tests =
  [ testGroup "Challenge 27"
    [ repeatTestCase "break CBC with iv = key" 10 $ do
        key <- randomBytes 16
        let text = "some text which will be encrypted and then, " <>
                   "assuming all goes well, validated successfully"
            ct = weakCBC key text
        breakWeakCBC (validateWeakCBC key) ct @?= Just key ]
  ]
