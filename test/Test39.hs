-- Tests for Challenge 39.
-- Checks that RSA encryption-decryption
-- works in both directions.

module Test39 ( tests ) where

import Bytes ( HasBytes(..) )
import Random ( randomBytes )
import PublicKey ( KeyPair(..) )
import PublicKey.RSA ( genRSAKeyPair, cryptRSA )

import TestFramework

tests =
  [ testGroup "Challenge 39"
    [ repeatTestCase "Encrypt public, decrypt private" 10 $ do
        kp <- genRSAKeyPair 192
        msg <- randomBytes 16
        let ciphertext = cryptRSA (kpPublic kp) msg
        cryptRSA (kpPrivate kp) ciphertext @?= fromBytes msg
    , repeatTestCase "Encrypt private, decrypt public" 10 $ do
        kp <- genRSAKeyPair 192
        msg <- randomBytes 16
        let ciphertext = cryptRSA (kpPrivate kp) msg
        cryptRSA (kpPublic kp) ciphertext @?= fromBytes msg
    ]
  ]
