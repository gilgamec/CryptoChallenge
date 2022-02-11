-- Tests for Challenge 64.
-- Checks that the truncated GCM-MAC works for validation;
-- then checks that we can find the key in a 2-byte truncation.

module Test64 ( tests ) where

import Bytes ( HasBytes(..), convBytes )
import Random ( randomBytes )
import Hash ( MAC(..) )
import AES ( encryptECB )

import Challenge64 ( gcmCTRTrunc, decryptGCMCTRTrunc, findH )

import Data.Maybe ( isJust )

import qualified Data.ByteString as B

import TestFramework

tests =
  [ testGroup "Challenge 64"
    [ repeatTestCase "truncated GCMCTR validates correctly" 10 $ do
        key <- randomBytes 16
        nonce <- randomBytes 12
        let ad = "from: bob to: alice"
            text = "my secret communications"
            mac = gcmCTRTrunc 4 key nonce ad text
        decryptGCMCTRTrunc 4 key mac @?= Just (ad, toBytes text)
    , testCase "find key in 2-byte truncated GCMCTR" $ do
        let trunc = 2
        key <- randomBytes 16
        nonce <- randomBytes 12
        let blockSize = 16
            msgLen = blockSize * 2^(4*trunc + 1)
        msg <- randomBytes msgLen
        let mac = gcmCTRTrunc trunc key nonce "" msg
            (_,_,ct) = macMessage mac
            oracle t = isJust $ decryptGCMCTRTrunc trunc key mac{ macMessage = (nonce,"",t) }
        h <- findH (trunc*8) oracle ct
        let Just realH = encryptECB key (B.replicate blockSize 0)
        convBytes h @?= realH
    ]
  ]
