-- Test for Challenge 3.
-- Checks that the key to the given monoalphabetic substitution cipher
-- is found correctly.

module Test3 ( tests ) where

import Bytes ( HasBytes(..) )
import Bytes.Hex ( mkHex )
import XORCipher ( breakMonoXOR )
import Distribution.English ( englishDistribution )

import Data.Maybe ( fromJust )

import TestFramework

-- The ciphertext, in hex encoding.
ctext = fromJust $ mkHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

tests =
  [ testGroup "Challenge 3" $
    [ testCase "correct decryption" $ do
        let (decrypted, key) = breakMonoXOR englishDistribution ctext
        key @?= 88 ]
  ]
