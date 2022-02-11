-- Tests for Challenge 26.
-- Check our sanitizer functions, then check that we can fake
-- admin credentials using bitflipping.

module Test26 ( tests ) where

import Random ( randomBytes )

import Challenge26 ( mkProfile, isAdmin, fakeAdmin )

import Data.Maybe ( isNothing, fromJust )

import TestFramework

tests =
  [ testGroup "Challenge 26"
    [ testCase "correctly sanitize crude fakery" $ do
        key <- randomBytes 16
        nonce <- randomBytes 8
        let crudeFake = "randomuserdata&role=admin"
        isNothing(mkProfile key nonce crudeFake) @? "crude fakery possible"
    , repeatTestCase "correctly fake admin credentials" 10 $ do
        key <- randomBytes 16
        nonce <- randomBytes 8
        let fakeProfile = fakeAdmin (mkProfile key nonce)
        isAdmin key nonce fakeProfile @? "did not fake admin credentials" ]
  ]
