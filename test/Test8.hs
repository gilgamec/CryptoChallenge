-- Test for Challenge 8.
-- Identify the index of the ECB-encrypted text.

module Test8 ( tests ) where

import Util ( argmax, countRepeats )
import Bytes ( HasBytes(..), chunksOf )
import Bytes.Base64 ( mkBase64 )

import Data.Maybe ( mapMaybe )
import qualified Data.ByteString as B

import TestFramework

-- Input data file
inputFileName = "data/08.txt"

tests =
  [ testGroup "Challenge 8"
    [ testCase "correct identification of input line" $ do
        ls <- mapMaybe mkBase64 . lines <$> readFile inputFileName
        let ix = argmax (countRepeats . chunksOf 16 . (ls!!)) [0..length ls - 1]
        ix @?= 132
    ]
  ]
