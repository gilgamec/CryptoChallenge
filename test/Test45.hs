-- Tests for Challenge 45.
-- Check that signatures are trivial to forge by setting g to 0 or 1.
-- (Though my validator won't accept the former, as r == 0 is not allowed.)

module Test45 ( tests ) where

import PublicKey ( publicPart )
import PublicKey.DSA ( DSAParams(..), genDSAParams, genDSAKeyPair,
                       dsaVerify )

import Challenge45 ( dsaZeroG, dsaZeroGForge, dsaP1G, dsaP1GForge )

import TestFramework

tests =
  [ testGroup "Challenge 45"
    [ repeatTestCase "(can't) forge with g = 0" 10 $ do
        params <- dsaZeroG <$> genDSAParams 512 80
        pubKey <- publicPart <$> genDSAKeyPair params
        let sig = dsaZeroGForge pubKey
        -- Note that the verifier will actually fail this one
        -- because r == 0.
        not (dsaVerify pubKey "Hello, world" sig) @?
          "Verifier somehow validated signature with r == 0"

    , repeatTestCase "forge with g = p + 1" 10 $ do
        params <- dsaP1G <$> genDSAParams 512 80
        pubKey <- publicPart <$> genDSAKeyPair params
        let sig = dsaP1GForge pubKey
        dsaVerify pubKey "Goodbye, world" sig @?
          "Could not forge with g = p+1"
    ]
  ]
