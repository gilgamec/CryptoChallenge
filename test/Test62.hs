-- Tests for Challenge 62.
-- Checks that weakened ECDSA works;
-- then check that we can break it with both 16- and 8-bit redundancy.

module Test62 ( tests ) where

import Random ( randomBytesR )
import EllipticCurve ( WECParameters(..), WECPoint, mkWECPoint )
import PublicKey ( KeyPair(..), publicPart )
import PublicKey.ECDSA ( WECDSAParams(..), genWECDSAKeyPair
                       , wecdsaVerifySignature )

import Challenge62 ( wecdsaSignBroken, breakWeakWECDSA )

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
  [ testGroup "Challenge 62"
    [ repeatTestCase "weak WECDSA works correctly" 10 $ do
        kp <- genWECDSAKeyPair dsaParams
        msg <- randomBytesR (40,80)
        sig <- wecdsaSignBroken 8 kp msg
        wecdsaVerifySignature (publicPart kp) msg sig @?= True
    , repeatTestCase "break weak WECDSA (16-bit redundancy)" 10 $ do
        let bits = 16
        kp <- genWECDSAKeyPair dsaParams
        let getSig = do
              msg <- randomBytesR (40,80)
              sig <- wecdsaSignBroken bits kp msg
              pure (msg,sig)
        pk <- breakWeakWECDSA bits (publicPart kp) getSig
        pk @?= kpPrivate kp
    , testCase "break weak WECDSA (8-bit redundancy)" $ do
        let bits = 8
        kp <- genWECDSAKeyPair dsaParams
        let getSig = do
              msg <- randomBytesR (40,80)
              sig <- wecdsaSignBroken bits kp msg
              pure (msg,sig)
        pk <- breakWeakWECDSA bits (publicPart kp) getSig
        pk @?= kpPrivate kp
    ]
  ]
