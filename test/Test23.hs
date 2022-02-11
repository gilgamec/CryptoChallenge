-- Test for Challenge 23.
-- Make an MT RNG with a random seed, throw away a bunch of values,
-- clones the RNG, then check that the clone and the original return
-- the same value stream.

module Test23 ( tests ) where

import MersenneTwister ( mtSeed, mtExtract, mtExtractMany, cloneMT )

import Data.List ( unfoldr )

import qualified Control.Monad.Random as R

import TestFramework

tests =
  [ testGroup "Challenge 23"
    [ repeatTestCase "clone an MT state" 10 $ do
        mt0 <- mtSeed <$> R.getRandom
        n <- R.getRandomR (0,1000)  -- throw away a bunch of values
        let (_,mt) = mtExtractMany n mt0
            vals = unfoldr (Just . mtExtract) mt
            cmt = cloneMT vals
        fst (mtExtractMany 5000 cmt) @?= take 5000 vals ]
  ]
