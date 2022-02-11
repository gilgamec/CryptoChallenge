-- Test for Challenge 65.
-- Checks that we can use length extension to forge GCMCTR.

module Test65 ( tests ) where

import Bytes ( convBytes )
import Random ( randomBytes, randomBytesR )
import AES ( encryptECB )

import Challenge64 ( gcmCTRTrunc, decryptGCMCTRTrunc )
import Challenge65 ( findH )

import Data.Maybe ( isJust )

import qualified Data.ByteString as B

import TestFramework

tests =
  [ testGroup "Challenge 65"
    [ testCase "find key in 2-byte truncated GCMCTR with length extension" $ do
        let trunc = 2
        key <- randomBytes 16
        let blockSize = 16
            msgLen = blockSize * 2^(4*trunc)
            mkMAC = do
              nonce <- randomBytes 12
              msg <- randomBytesR (msgLen - 15,msgLen - 1)
              pure $ gcmCTRTrunc trunc key nonce "" msg
            oracle = isJust . decryptGCMCTRTrunc trunc key
        h <- findH (trunc*8) mkMAC oracle
        let Just realH = encryptECB key (B.replicate blockSize 0)
        convBytes h @?= realH
    ]
  ]
