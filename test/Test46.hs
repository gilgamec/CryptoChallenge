-- Test for Challenge 46.
-- Checks that a parity oracle lets us decrypt the secret message.

module Test46 ( tests ) where

import Bytes ( convBytes )
import Bytes.Base64 ( Base64, mkBase64 )
import PublicKey ( KeyPair(..), publicPart )
import PublicKey.RSA ( genRSAKeyPair, cryptRSA )

import Challenge46 ( rsaParityOracle, rsaParityDecrypt )

import Data.Maybe ( fromJust )

import TestFramework

-- Plaintext from the Challenge description.
plaintext :: Base64
plaintext = fromJust . mkBase64 $
  "VGhhdCdzIHdoeSBJIGZvdW5kIHlvdSBkb24ndCBwbGF5IGFyb3VuZCB3aXRoIHRoZSBGdW5reSBDb2xkIE1lZGluYQ=="

tests =
  [ testGroup "Challenge 46"
    [ repeatTestCase "parity oracle decryption" 10 $ do
        kp <- genRSAKeyPair 640
        let pubKey = publicPart kp
            oracle = rsaParityOracle kp
            ctext = cryptRSA (kpPublic kp) plaintext
            decrypts = rsaParityDecrypt pubKey oracle ctext
        last decrypts @=? convBytes plaintext
    ]
  ]
