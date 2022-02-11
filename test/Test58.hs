-- Tests for Challenge 58.
-- Checks that the kangaroo chase lets us find the discrete logarithm
-- with a small or a largeish bound.
-- Checks that we can then use it to find a private key.

module Test58 ( tests ) where

import PublicKey ( KeyPair(..), publicPart )
import PublicKey.DiffieHellman ( DHParams(..) )
import GroupOps ( kangarooChase )
import Modulo ( mkMod, getVal, modulo, (^%), withMod, MultMod(..), mkMultMod )

import Challenge57 ( genDHKeyPair', bobEncoder )
import Challenge58 ( getPrivateKey )

import TestFramework

-- The DH parameters from the Challenge text
params :: DHParams
params = DHParams
  { dhModulus = 11470374874925275658116663507232161402086650258453896274534991676898999262641581519101074740642369848233294239851519212341844337347119899874391456329785623
  , dhGenerator = 622952335333961296978159266084741085889881358738459939978290179936063635566740258555167783009058567397963466103140082647486611657350811560630587013183357 }

setQ :: Integer
setQ = 335062023296420808191071248367701059461

-- The sample discrete logarithm candidates, from the Challenge text
ySmall :: Integer
ySmall = 7760073848032689505395005705677365876654629189298052775754597607446617558600394076764814236081991643094239886772481052254010323780165093955236429914607119

yBig :: Integer
yBig = 9388897478013399550694114614498790691034187453089355259602614074132918843899833277397448144245883225611726912025846772975325932794909655215329941809013733


tests =
  [ testGroup "Challenge 58"
    [ testCase "discrete log of y, in small range" $ do
        let DHParams p g = params
            cat = fromInteger . getVal . getMultMod -- categorization = value
            Just dlog = withMod p (\y -> kangarooChase cat
                                            (mkMultMod g) y (0,2^20))
                                  (mkMultMod ySmall)
        ((mkMod g ^% dlog) `modulo` p) @?= ySmall
    , testCase "discrete log of y, in large range" $ do
        let DHParams p g = params
            cat = fromInteger . getVal . getMultMod -- categorization = value
            Just dlog = withMod p (\y -> kangarooChase cat
                                            (mkMultMod g) y (0,2^40))
                                  (mkMultMod yBig)
        ((mkMod g ^% dlog) `modulo` p) @?= yBig
    , testCase "reassemble Bob's private key" $ do
        bobKey <- genDHKeyPair' params setQ
        let oracle = bobEncoder bobKey "miscellaneous '90s rap lyrics"
        prv <- getPrivateKey oracle (publicPart bobKey) setQ
        prv @?= kpPrivate bobKey
    ]
  ]
