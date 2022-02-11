-- Tests for Challenge 43.
-- Check that the DSA scheme works correctly;
-- then checks that we can find the private key from a 16-bit k.

module Test43 ( tests ) where

import Bytes ( HasBytes(..), Bytes, convBytes )
import Bytes.Hex ( mkHex, showHex )
import Hash ( sha1Hash )
import Random ( randomBytesR )
import PublicKey ( PublicKey(..), publicPart )
import PublicKey.DSA ( DSAParams(..), genDSAParams, DSAPublicKey, genDSAKeyPair
                     , dsaSign, dsaVerify )

import Challenge43 ( dsaBreakBadK )

import Data.Maybe ( fromJust )

import TestFramework

-- The DSA parameters given in the Challenge.
dsparams :: DSAParams
dsparams = DSAParams
  { dsaP = convBytes . fromJust . mkHex . concat $
    [ "800000000000000089e1855218a0e7dac38136ffafa72eda7"
    , "859f2171e25e65eac698c1702578b07dc2a1076da241c76c6"
    , "2d374d8389ea5aeffd3226a0530cc565f3bf6b50929139ebe"
    , "ac04f48c3c84afb796d61e5a4f9a8fda812ab59494232c7d2"
    , "b4deb50aa18ee9e132bfa85ac4374d7f9091abc3d015efc87"
    , "1a584471bb1" ]
  , dsaQ = convBytes . fromJust . mkHex $
    "f4f47f05794b256174bba6e9b396a7707e563c5b"
  , dsaG = convBytes . fromJust . mkHex . concat $
    [ "5958c9d3898b224b12672c0b98e06c60df923cb8bc999d119"
    , "458fef538b8fa4046c8db53039db620c094c9fa077ef389b5"
    , "322a559946a71903f990f1f7e0e025e2d7f7cf494aff1a047"
    , "0f5b64c36b625a097f1651fe775323556fe00b3608c887892"
    , "878480e99041be601a62166ca6894bdd41a7054ec89f756ba"
    , "9fc95302291" ]
  }

-- The DSA public key given in the Challenge.
publicKey :: DSAPublicKey
publicKey = PublicKey
  { pkParameters = dsparams
  , pkKey = convBytes . fromJust . mkHex . concat $
    [ "84ad4719d044495496a3201c8ff484feb45b962e7302e56a392aee4"
    , "abab3e4bdebf2955b4736012f21a08084056b19bcd7fee56048e004"
    , "e44984e2f411788efdc837a0d2e5abb7b555039fd243ac01f0fb2ed"
    , "1dec568280ce678e931868d23eb095fde9d3779191b8c0299d6e07b"
    , "bb283e6633451e535c45513b2d33c99ea17" ] }

-- The message, checksum, and signature given in the Challenge.
message :: String
message = unlines
  [ "For those that envy a MC it can be hazardous to your health"
  , "So be friendly, a matter of life and death, just like a etch-a-sketch" ]

message_checksum :: Bytes
message_checksum = toBytes . fromJust . mkHex $
  "d2d0714f014a9784047eaeccf956520045c45265"

message_signature :: (Integer,Integer)
message_signature =
  ( 548099063082341131477253921760299949438196259240
  , 857042759984254168557880549501802188789837994940 )

-- The SHA-1 hash of the private key used to sign the message
privateKey_checksum :: Bytes
privateKey_checksum = toBytes . fromJust . mkHex $
  "0954edd5e0afe5542a4adf012611a91912a3ec16"


tests =
  [ testGroup "Challenge 43"
    [ repeatTestCase "DSA scheme works correctly" 10 $ do
        kp <- genDSAParams 512 80 >>= genDSAKeyPair
        msg <- randomBytesR (40,80)
        sig <- dsaSign kp msg
        dsaVerify (publicPart kp) msg sig @? "could not verify signature"
    , testCase "given message matches SHA-1 checksum" $ do
        sha1Hash message @=? fromBytes message_checksum
    , testCase "private key found correctly" $ do
        let prv = dsaBreakBadK publicKey message message_signature
        sha1Hash (showHex prv) @=? fromBytes privateKey_checksum
    ]
  ]
