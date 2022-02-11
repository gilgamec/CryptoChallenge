-- Tests for Challenge 61.
-- Checks signing and signature duplication in elliptic curve DSA;
-- then checks signature duplication in RSA.

module Test61 ( tests ) where

import Random ( randomBytesR )
import EllipticCurve ( WECParameters(..), WECPoint, mkWECPoint )
import PublicKey ( publicPart )
import PublicKey.ECDSA ( WECDSAParams(..), genWECDSAKeyPair
                       , wecdsaSign, wecdsaVerifySignature )
import PublicKey.RSA ( genRSAKeyPair, rsaSign, rsaVerifySignature )

import Challenge61 ( sigDuplicateWEC, sigDuplicateRSA )

import TestFramework

-- Parameters from Challenge 59.
params :: WECParameters
params = WECParameters { wecA = -95051, wecB = 11279326
                       , wecP = 233970423115425145524320034830162017933 }

base :: WECPoint
base = mkWECPoint params 182 85518893674295321206118380980485522083

order :: Integer
order = 233970423115425145498902418297807005944

q :: Integer
q = order `div` 8

dsaParams :: WECDSAParams
dsaParams = WECDSAParams{ wecdsaParams = params
                        , wecdsaG = base
                        , wecdsaQ = q }

tests =
  [ testGroup "Challenge 61"
    [ testGroup "Part 1: WECDSA"
      [ repeatTestCase "WECDSA works correctly" 10 $ do
          kp <- genWECDSAKeyPair dsaParams
          msg <- randomBytesR (40,80)
          sig <- wecdsaSign kp msg
          wecdsaVerifySignature (publicPart kp) msg sig @?= True
      , repeatTestCase "WECDSA signature duplication" 10 $ do
          kp <- genWECDSAKeyPair dsaParams
          msg <- randomBytesR (40,80)
          sig <- wecdsaSign kp msg
          fake_pk <- sigDuplicateWEC (publicPart kp) msg sig
          wecdsaVerifySignature fake_pk msg sig @?= True
      ]
    , testGroup "Part 2: RSA"
      [ repeatTestCase "RSA signature duplication" 10 $ do
          kp <- genRSAKeyPair 512
          msg <- randomBytesR (20,40)
          let Just sig = rsaSign kp msg
          rsaVerifySignature (publicPart kp) msg sig @?= True
          fakeKP <- sigDuplicateRSA (publicPart kp) msg sig
          rsaVerifySignature (publicPart fakeKP) msg sig @?= True
      ]
    ]
  ]
