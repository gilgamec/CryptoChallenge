-- Test for Challenge 11.
-- Generate a random AES cipher, either ECB or CBC,
-- and check that we can identify it correctly.

module Test11 ( tests ) where

import Challenge11 ( randomAESCipher, ecbDetector )

import Control.Monad ( replicateM )

import qualified Data.ByteString as B

import TestFramework

tests =
  [ testGroup "Challenge 11"
    [ repeatTestCase "correctly identify a random scheme" 100 $ do
        (isECB,cipher) <- randomAESCipher
        ecbDetector 16 cipher @?= isECB
    ]
  ]
