-- Tests for Challenge 41.
-- Checks that the oracle *normally* rejects decrypting the same message twice;
-- check that we can break that by multiplying by a random number.

module Test41 ( tests ) where

import Bytes ( HasBytes(..) )
import Random ( randomBytes )
import PublicKey ( KeyPair(..), publicPart )
import PublicKey.RSA ( genRSAKeyPair, cryptRSA, RSAKeyPair )

import Challenge41 ( breakUnpadded )

import Data.Maybe ( isNothing )

import TestFramework

oracle :: RSAKeyPair -> [Integer] -> Integer -> Maybe Integer
oracle kp cts ct
  | ct `elem` cts = Nothing
  | otherwise     = Just $ cryptRSA (kpPrivate kp) ct

tests =
  [ testGroup "Challenge 41"
    [ repeatTestCase "can't double-request from oracle" 10 $ do
        kp <- genRSAKeyPair 192
        msg <- randomBytes 16
        let ciphertext = cryptRSA (kpPublic kp) msg
            oracleF = oracle kp [ciphertext]
        isNothing (oracleF ciphertext) @? "a double request slipped through"
    , repeatTestCase "can break unpadded message" 10 $ do
        kp <- genRSAKeyPair 192
        msg <- randomBytes 16
        let ciphertext = cryptRSA (kpPublic kp) msg
            oracleF = oracle kp [ciphertext]
        breakUnpadded oracleF (publicPart kp) ciphertext @?= Just (fromBytes msg)
    ]
  ]
