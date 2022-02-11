-- Tests for Challenge 20 (and 19, kinda).
-- Uses breakMultiXOR to statistically decrypt a big chunk of the input strings
-- and checks that they are just the given strings.

module Test20 ( tests ) where

import Bytes ( HasBytes(..), convBytes )
import Bytes.Base64 ( Base64, mkBase64 )
import Random ( randomBytes )
import AES ( encryptCTR, aesCTRBlocks )
import XORCipher ( breakMultiXOR )
import Distribution.English ( englishDistribution )

import Data.Char ( toLower )
import Data.Maybe ( mapMaybe )

import qualified Data.ByteString as B

import TestFramework

mysteryLines :: [Base64]
mysteryLines = mapMaybe mkBase64
  [ "SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ=="
  , "Q29taW5nIHdpdGggdml2aWQgZmFjZXM="
  , "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ=="
  , "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4="
  , "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk"
  , "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
  , "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ="
  , "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA=="
  , "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU="
  , "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl"
  , "VG8gcGxlYXNlIGEgY29tcGFuaW9u"
  , "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA=="
  , "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk="
  , "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg=="
  , "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo="
  , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
  , "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA=="
  , "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA=="
  , "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA=="
  , "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg=="
  , "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw=="
  , "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA=="
  , "U2hlIHJvZGUgdG8gaGFycmllcnM/"
  , "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w="
  , "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4="
  , "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ="
  , "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs="
  , "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA=="
  , "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA=="
  , "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4="
  , "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA=="
  , "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu"
  , "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc="
  , "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs"
  , "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs="
  , "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0"
  , "SW4gdGhlIGNhc3VhbCBjb21lZHk7"
  , "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw="
  , "VHJhbnNmb3JtZWQgdXR0ZXJseTo="
  , "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4="
  ]

-- The XOR breaker isn't really good at solving when all of the letters
-- are capitalized; it prefers lower-case over upper.
-- We thus check its work by checking without case.
assertEqCaseless :: (HasBytes a, HasBytes b) => a -> b -> Assertion
assertEqCaseless s1 s2 = mkLC s1 @?= mkLC s2
 where
  mkLC :: HasBytes a => a -> String
  mkLC = map toLower . convBytes

tests =
  [ testGroup "Challenge 20"
    [ repeatTestCase "decrypt challenge 19 strings" 5 $ do
        key <- randomBytes 16
        nonce <- randomBytes 16
        let Just keystream = aesCTRBlocks key nonce 0
            minLen = minimum (map numBytes mysteryLines)
            textsIn = map (B.take minLen . toBytes) mysteryLines
        let cts = mapMaybe (encryptCTR key nonce) textsIn
            (textsOut,kbs) = breakMultiXOR englishDistribution cts
        sequence_ $ zipWith assertEqCaseless textsIn textsOut
    , testCase "decrypt data file" $ do
        key <- randomBytes 16
        nonce <- randomBytes 16
        texts <- mapMaybe mkBase64 . lines <$> readFile "data/20.txt"
        let Just keystream = aesCTRBlocks key nonce 0
            minLen = minimum (map numBytes texts)
            textsIn = map (B.take minLen . toBytes) texts
        let cts = mapMaybe (encryptCTR key nonce) textsIn
            (textsOut,kbs) = breakMultiXOR englishDistribution cts
        sequence_ $ zipWith assertEqCaseless textsIn textsOut
    ]
  ]
