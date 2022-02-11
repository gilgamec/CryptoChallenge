-- Test for Challenge 21.
-- Checks that my MT19937 implementation
-- gives the same results as a reference version.

module Test21 ( tests ) where

import MersenneTwister ( mtSeed, mtExtract )

import Data.List ( unfoldr )
import Data.Word ( Word32 )

import TestFramework

-- The site [http://create.stephan-brumme.com/mersenne-twister/]
-- has an implementation which reports the following values for
-- the first ten elements with the given seed:
seed :: Word32
seed = 5489

values :: [Word32]
values =
  [ 0xD091BB5C, 0x22AE9EF6, 0xE7E1FAEE, 0xD5C31F79, 0x2082352C
  , 0xF807B7DF, 0xE9D30005, 0x3895AFE1, 0xA1E24BBA, 0x4EE4092B
  ]

tests =
  [ testGroup "Challenge 21"
    [ testCase "match known random values" $ let
        mtValues = unfoldr (Just . mtExtract) (mtSeed seed)
       in
        take (length values) mtValues @?= values ]
  ]
