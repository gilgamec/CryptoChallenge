-- Tests for Challenge 6.
-- Check that the Hamming distance is computed correctly;
-- then check that the polyalphabetic XOR cipher in the input file
-- can be broken, and the derived key is in fact the actual key.

module Test6 ( tests ) where

import Bytes ( HasBytes(..) )
import Bytes.Base64 ( mkBase64 )
import XORCipher ( hammingDistance, breakPolyXOR )
import Distribution.English ( englishDistribution )

import TestFramework

-- Input strings to test Hamming distance
str1 = "this is a test"
str2 = "wokka wokka!!!"

-- Input data file
inputFileName = "data/06.txt"

-- Correct key
realKey = "Terminator X: Bring the noise"

tests =
  [ testGroup "Challenge 6"
    [ testCase "correct Hamming distance" $ hammingDistance str1 str2 @?= 37
    , testCase "correct decryption of input file" $ do
        Just bs <- mkBase64 . concat . lines <$> readFile inputFileName
        let (decrypt,key) = breakPolyXOR englishDistribution bs
        key @?= toBytes realKey
    ]
  ]
