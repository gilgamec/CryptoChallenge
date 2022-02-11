-- Test for Challenge 5.
-- Check that the given plaintext encrypts to the given ciphertext
-- under polyalphabetic XOR with the given key.

module Test5 ( tests ) where

import Bytes ( HasBytes(..) )
import Bytes.Hex ( mkHex )
import XORCipher ( polyXOR )

import TestFramework

-- The plaintext to encrypt
plaintext = "Burning 'em, if you ain't quick and nimble\n" ++
            "I go crazy when I hear a cymbal"

-- The key to encrypt under
key = "ICE"

-- Hex encoding of he expected ciphertext
Just ctext = mkHex $
  "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272" ++
  "a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"

tests =
  [ testGroup "Challenge 5" $
    [ testCase "correct encryption" $
        polyXOR key plaintext @?= toBytes ctext ]
  ]
