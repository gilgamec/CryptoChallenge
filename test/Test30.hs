-- Test for Challenge 30.
-- Check that we can trivially break the secret-prefix MAC
-- to give ourselves admin credentials.

module Test30 ( tests ) where

import Random ( randomBytesR )

import Challenge30 ( mkProfile, isAdmin, fakeAdmin )

import Data.Maybe ( fromJust )

import TestFramework

tests =
  [ testGroup "Challenge 30"
    [ repeatTestCase "correctly fake admin credentials" 10 $ do
        key <- randomBytesR (8,20)
        let Just realProfile = mkProfile key "some_user_data"
            fakeProfile = fakeAdmin (isAdmin key) realProfile
        isAdmin key fakeProfile @? "could not fake admin credentials" ]
  ]
