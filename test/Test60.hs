-- Tests for Challenge 60.
-- First checks that elliptic curves in the Montgomery formulation
-- can also perform Diffie-Hellman secret sharing.
-- Then checks that we can still break it, using a twist curve.

module Test60 ( testsFast, testsSlow ) where

import EllipticCurve ( WECParameters(..), mkWECPoint
                     , MECParameters(..), MECPoint )
import PublicKey ( KeyPair(..), publicPart )
import PublicKey.ECDiffieHellman ( MECDHParams(..)
                                 , genMECDHKeyPair, mecdhSharedSecret )

import Challenge60 ( bobEncoderMEC, getPrivateKey, vsForU )

import TestFramework

-- Parameters from Challenge text
mParams :: MECParameters
mParams = MECParameters { mecA = 534, mecB = 1
                        , mecP = 233970423115425145524320034830162017933 }

mBase :: MECPoint
mBase = 4

wParams :: WECParameters
wParams = WECParameters { wecA = -95051, wecB = 11279326
                        , wecP = 233970423115425145524320034830162017933 }

order :: Integer
order = 233970423115425145498902418297807005944

q :: Integer
q = order `div` 8

testsFast =
  [ testGroup "Challenge 60"
    [ repeatTestCase "test mec diffie hellman" 10 $ do
        kp1 <- genMECDHKeyPair (MECDHParams mParams mBase q)
        kp2 <- genMECDHKeyPair (MECDHParams mParams mBase q)
        mecdhSharedSecret kp1 (publicPart kp2) @?=
          mecdhSharedSecret kp2 (publicPart kp1)
    ]
  ]

testsSlow =
  [ testGroup "Challenge 60"
    [ testCase "test twist attack" $ do
        bobKey <- genMECDHKeyPair (MECDHParams mParams mBase q)
        let pubKey = publicPart bobKey
            oracle = bobEncoderMEC bobKey "an important message"
            m2ws u = mkWECPoint wParams (u+178) <$> vsForU mParams u
        p <- getPrivateKey oracle pubKey order wParams m2ws
        p @?= kpPrivate bobKey
    ]
  ]
