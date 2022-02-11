-- Test for Challenge 40.
-- Checks that we can find the message if it is sent
-- three times with three different public keys.

module Test40 ( tests ) where

import Bytes ( HasBytes(..) )
import Random ( randomBytes )
import PublicKey ( KeyPair(..) )
import PublicKey.RSA ( genRSAKeyPair, cryptRSA )

import Challenge40 ( broadcastAttack )

import TestFramework

import Control.Monad ( replicateM )

tests =
  [ testGroup "Challenge 40"
    [ repeatTestCase "find multiply-encrypted message" 10 $ do
        msg <- randomBytes 16
        cns <- replicateM 3 $ do
          kp <- genRSAKeyPair 192
          let ciphertext = cryptRSA (kpPublic kp) msg
          pure (ciphertext, snd $ kpPublic kp)
        broadcastAttack cns @?= fromBytes msg
    ]
  ]
