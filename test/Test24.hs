-- Tests for Challenge 24.
-- Checks the use of MT as a stream cipher, then checks that it can be broken.
-- Checks using MT to create a password token, then checks that
-- the token can be recognized.

module Test24 ( tests ) where

import Random ( randomBytes, randomBytesR )

import Challenge24 ( mtXOR, mtEncrypt16, breakMTEncrypt16
                   , resetToken, isResetToken )

import qualified Control.Monad.Random as R

import Data.Time.Clock.POSIX ( getPOSIXTime )

import TestFramework

tests =
  [ testGroup "Challenge 24"
    [ repeatTestCase "test the MT19936 stream cipher" 10 $ do
        key <- R.getRandom
        text <- randomBytesR (200,400)
        let ct = mtXOR key text
        mtXOR key ct @?= text
    , testCase "break the MT19936 stream cipher" $ do
        key <- R.getRandom
        prefix <- randomBytesR (20,40)
        breakMTEncrypt16 (mtEncrypt16 key prefix) @?= key
    , repeatTestCase "identify a password token" 5 $ do
        token <- resetToken
        now <- getPOSIXTime
        delay <- R.getRandomR (120,1200) :: IO Double
        isResetToken token (now + realToFrac delay) @?
          "could not identify a password token"
    , repeatTestCase "don't identify a random string as a password token" 5 $ do
        bytes <- randomBytes 4
        now <- getPOSIXTime
        delay <- R.getRandomR (120,1200) :: IO Double
        not (isResetToken bytes (now + realToFrac delay)) @?
          "identified random string as password token" ]
  ]
