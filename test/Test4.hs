-- Test for Challenge 4.
-- Reads in the input file, detects the single line encrypted
-- with monoalphabetic XOR, and checks that it decrypts as expected.

module Test4 ( tests ) where

import Util ( argmax )
import Bytes ( HasBytes(..) )
import Bytes.Hex ( mkHex )
import XORCipher ( breakMonoXOR )
import Distribution ( countBytes, logLikelihood )
import Distribution.English ( englishDistribution )

import Data.Maybe ( mapMaybe )

import TestFramework

-- Input data file
inputFileName = "data/04.txt"

-- Correct plaintext
ptext = "Now that the party is jumping\n"

tests =
  [ testGroup "Challenge 4"
    [ testCase "decryption of the right line" $ do
        lines <- mapMaybe mkHex . lines <$> readFile inputFileName
        let bestEach = map (fst . breakMonoXOR englishDistribution) lines
            best = argmax (logLikelihood englishDistribution .
                           countBytes) bestEach
        best @?= toBytes ptext
    ]
  ]
