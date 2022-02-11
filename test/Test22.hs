-- Test for Challenge 22.
-- Check that we can find the MT seed after a few seconds.

module Test22 ( tests ) where

import MersenneTwister ( mtSeed, mtExtract )

import Challenge22 ( crackMTSeed )

import Data.Time.Clock.POSIX ( getPOSIXTime )

import qualified Control.Monad.Random as R

import TestFramework

tests =
  [ testGroup "Challenge 22"
    [ repeatTestCase "duplicate time-seeded rng" 10 $ do
        seedNow <- floor <$> getPOSIXTime
        let val = fst . mtExtract . mtSeed $ seedNow
        -- Ordinarily, we'd wait, but that's boring. But apparently more "fun"?
        waitTime <- R.getRandomR (40,1000) :: IO Double
        time <- (realToFrac waitTime +) <$> getPOSIXTime
        crackMTSeed val time @?= seedNow ]
  ]
