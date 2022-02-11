-- Tests for Challenge 66.
-- Checks that we can use implementation errors at a rate of on in 100,000
-- to find private keys, in both reduced and full bit-width.

module Test66 ( testsFast, testsSlow ) where

import EllipticCurve ( WECPoint, mkWECPoint )
import PublicKey ( KeyPair(..), publicPart )
import PublicKey.ECDiffieHellman ( WECDHParams(..), genWECDHKeyPair )

import Challenge66 ( BWECParameters(..), wecParamsFor
                   , bwecOracle, findBWECPrivateKey )

import TestFramework

-- Parameters from Challenge 59.
params :: BWECParameters
params = BWECParameters { bwecA = -95051, bwecB = 11279326
                        , bwecP = 233970423115425145524320034830162017933
                        , bwecFail = 100000 }

base :: WECPoint
base = mkWECPoint (wecParamsFor params)
                  182 85518893674295321206118380980485522083

order :: Integer
order = 233970423115425145498902418297807005944

q :: Integer
q = order `div` 8

testsFast =
  [ testGroup "Challenge 66"
    [ testCase "find 20-bit private key" $ do
        let dhParams = WECDHParams (wecParamsFor params) base (2^20)
        bobKey <- genWECDHKeyPair dhParams
        let oracle = bwecOracle params bobKey
        d <- findBWECPrivateKey params oracle (publicPart bobKey)
        d @?= kpPrivate bobKey 
    ]
  ]

testsSlow =
  [ testGroup "Challenge 66"
    [ testCase "find full-width private key" $ do
        let dhParams = WECDHParams (wecParamsFor params) base q
        bobKey <- genWECDHKeyPair dhParams
        let oracle = bwecOracle params bobKey
        d <- findBWECPrivateKey params oracle (publicPart bobKey)
        d @?= kpPrivate bobKey 
    ]
  ]
