-- Tests for Challenge 16.
-- Checks that the profile creator correctly sanitizes input strings;
-- then checks that we can still fake admin credentials.

module Test16 ( tests ) where

import Challenge16 ( mkProfile, isAdmin, fakeAdmin )

import Random ( randomBytes )

import Data.Maybe ( isNothing, fromJust )

import TestFramework

tests =
  [ testGroup "Challenge 16"
    [ testCase "correctly sanitize crude fakery" $ do
        key <- randomBytes 16
        iv <- randomBytes 16
        let crudeFake = "randomuserdata&role=admin"
        isNothing (mkProfile key iv crudeFake) @? "crude fakery successful"
    , repeatTestCase "correctly fake admin credentials" 10 $ do
        key <- randomBytes 16
        iv <- randomBytes 16
        let fakeProfile = fakeAdmin (mkProfile key iv) (isAdmin key iv)
        isAdmin key iv fakeProfile @? "could not fake admin credentials" ]
  ]
