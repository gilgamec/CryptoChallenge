-- Tests for Challenge 42.
-- Checks that RSA signatures work normally, and that the broken validator also works.
-- Checks that the broken validator can be faked out by the cube-root method,
-- but the correct validator isn't fooled.

module Test42 ( tests ) where

import Random ( randomBytes )
import PublicKey ( publicPart )
import PublicKey.RSA ( genRSAKeyPair, rsaSign, rsaVerifySignature )

import Challenge42 ( rsaVerifySignature_Broken, fakeRSASignature )

import TestFramework

tests =
  [ testGroup "Challenge 42"
    [ repeatTestCase "RSA signatures work" 10 $ do
        kp <- genRSAKeyPair 1024
        msg <- randomBytes 16
        let Just sig = rsaSign kp msg
        rsaVerifySignature (publicPart kp) msg sig @? "signatures don't work"
    , repeatTestCase "Broken validator works naively" 10 $ do
        kp <- genRSAKeyPair 1024
        msg <- randomBytes 16
        let Just sig = rsaSign kp msg
        rsaVerifySignature_Broken (publicPart kp) msg sig @?
          "broken validator doesn't work"
    , repeatTestCase "Broken validator can be faked out" 10 $ do
        kp <- genRSAKeyPair 1024
        msg <- randomBytes 16
        let sig = fakeRSASignature (publicPart kp) msg
        rsaVerifySignature_Broken (publicPart kp) msg sig @?
          "broken validator couldn't be fooled"
    , repeatTestCase "Correct validator isn't fooled" 10 $ do
        kp <- genRSAKeyPair 1024
        msg <- randomBytes 16
        let sig = fakeRSASignature (publicPart kp) msg
        not (rsaVerifySignature (publicPart kp) msg sig) @?
          "rsa validator fooled by cube-root trick"
    ]
  ]
