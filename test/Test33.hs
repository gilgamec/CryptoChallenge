-- Test for Challenge 33.
-- Checks that Diffie-Hellman secret-sharing works.

module Test33 ( tests ) where

import PublicKey ( publicPart )
import PublicKey.DiffieHellman ( DHParams(..), genDHKeyPair, dhSharedSecret
                               , prime1536 )

import TestFramework

tests =
  [ testGroup "Challenge 33"
    [ repeatTestCase "shared secret is really shared" 10 $ do
        let params = DHParams prime1536 2
        kp1 <- genDHKeyPair params
        kp2 <- genDHKeyPair params
        dhSharedSecret kp1 (publicPart kp2) @?=
          dhSharedSecret kp2 (publicPart kp1)
    ]
  ]
