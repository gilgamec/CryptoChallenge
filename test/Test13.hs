-- Tests for Challenge 13.
-- Check that our mkProfile function sanitizes email addresses and
-- won't allow the creation of accounts with admin privileges;
-- then check that we can fake them just fine.

module Test13 ( tests ) where

import Random ( randomBytes )

import Challenge13 ( mkProfile, userRole, fakeAdmin )

import Data.Maybe ( isNothing, fromJust )

import TestFramework

tests =
  [ testGroup "Challenge 13"
    [ testCase "correctly sanitize crude fakery" $ do
        key <- randomBytes 16
        let crudeFake = "myemail@example.com&role=admin"
        isNothing (mkProfile key crudeFake) @? "crude fakery possible"
    , repeatTestCase "correctly fake admin credentials" 10 $ do
        key <- randomBytes 16
        let fakeProfile = fakeAdmin (mkProfile key)
        userRole key fakeProfile @?= Just "admin" ]
  ]
