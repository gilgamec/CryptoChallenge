-- Tests for Challenge 63.
-- Checks that GCMCTR works as a MAC;
-- then check that repeating nonces lets us forge i.

module Test63 ( tests ) where

import Bytes ( HasBytes(..), convBytes )
import Hash ( MAC(..) )
import Random ( randomBytes )
import GCM ( gcmCTR, decryptGCMCTR  )

import Challenge63 ( findGCMKey, forgeGCM )

import Data.Maybe ( isJust, isNothing )

import TestFramework

tests =
  [ testGroup "Challenge 63"
    [ repeatTestCase "GCMCTR validates correctly" 10 $ do
        key <- randomBytes 16
        nonce <- randomBytes 12
        let ad = "from: bob to: alice"
            text = "my secret communications"
            mac = gcmCTR key nonce ad text
        decryptGCMCTR key mac @?= Just (ad,toBytes text)
    , testCase "can't change ad or ciphertext in GCMCTR" $ do
        key <- randomBytes 16
        nonce <- randomBytes 12
        let ad = "from: bob to: alice"
            text = "my secret communications"
            mac = gcmCTR key nonce ad text
            (_,_,ct) = macMessage mac
            evilMAC1 = mac{ macMessage = (nonce, "from: evil to: alice", ct) }
            evilMAC2 = mac{ macMessage = (nonce, ad
                                         ,convBytes "an evil ciphertext block") }
        isNothing (decryptGCMCTR key evilMAC1) @? "replaced ad"
        isNothing (decryptGCMCTR key evilMAC2) @? "replaced ct"
    , repeatTestCase "can force GCM with reused nonces" 10 $ do
        key <- randomBytes 16
        nonce <- randomBytes 12
        let mac1 = gcmCTR key nonce "to: alice from: bob" "secret message one"
            mac2 = gcmCTR key nonce "to: carol from: bob" "secret message two"
            oracle = isJust . decryptGCMCTR key
        (h,s) <- findGCMKey mac1 mac2 oracle
        let forgedMAC = forgeGCM h s ( nonce, "to: david from: bob, really"
                                     , convBytes "an evil ciphertext block" )
        isJust (decryptGCMCTR key forgedMAC) @? "could not forge a MAC"
    ]
  ]
