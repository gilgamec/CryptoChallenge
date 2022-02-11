-- Tests for Challenge 59.
-- Checks that Diffie-Hellman secret sharing works under elliptic curves.
-- Then checks that it can be broken using off-curve points.

module Test59 ( tests ) where

import EllipticCurve ( WECParameters(..), WECPoint, mkWECPoint )
import PublicKey ( KeyPair(..), publicPart )
import PublicKey.ECDiffieHellman ( WECDHParams(..)
                                 , genWECDHKeyPair, wecdhSharedSecret )

import Challenge59 ( bobEncoderWEC, getPrivateKey)

import TestFramework

-- Parameters from the Challenge text.
params :: WECParameters
params = WECParameters
  { wecA = -95051, wecB = 11279326
  , wecP = 233970423115425145524320034830162017933 }

base :: WECPoint
base = mkWECPoint params 182 85518893674295321206118380980485522083

order :: Integer
order = 233970423115425145498902418297807005944

q :: Integer
q = order `div` 8

-- Fake parameters from the Challenge text and their respective orders.
fakeparams :: [(WECParameters,Integer)]
fakeparams =
  [ ( params{ wecB = 210 }, 233970423115425145550826547352470124412 )
  , ( params{ wecB = 504 }, 233970423115425145544350131142039591210 )
  , ( params{ wecB = 727 }, 233970423115425145545378039958152057148 ) ]

tests =
  [ testGroup "Challenge 59"
    [ repeatTestCase "test wec diffie hellman" 10 $ do
        kp1 <- genWECDHKeyPair (WECDHParams params base q)
        kp2 <- genWECDHKeyPair (WECDHParams params base q)
        wecdhSharedSecret kp1 (publicPart kp2) @?=
          wecdhSharedSecret kp2 (publicPart kp1)
    , testCase "crack the private key with invalid curves" $ do
        bobKey <- genWECDHKeyPair (WECDHParams params base q)
        let pubKey = publicPart bobKey
            oracle = bobEncoderWEC bobKey "hip to the hop to the hip hip hop"
        x <- getPrivateKey oracle pubKey fakeparams
        x @?= kpPrivate bobKey
    ]
  ]
