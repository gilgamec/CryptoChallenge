-- Test for Challenge 44.
-- Checks that we can break the DSA key from signatures using repeated nonces.

module Test44 ( tests ) where

import Bytes ( HasBytes(..), Bytes, convBytes )
import Bytes.Hex ( mkHex, showHex )
import Hash ( sha1Hash )
import PublicKey ( PublicKey(..) )
import PublicKey.DSA ( DSAParams(..), DSAPublicKey )

import Challenge44 ( dsaBreakRepeatedK )

import Data.Maybe ( fromJust )

import TestFramework

-- The input data file.
filename :: FilePath
filename = "data/44.txt"

-- The DSA parameters from Challenge 43.
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

-- The public key given in the Challenge description.
publicKey :: DSAPublicKey
publicKey = PublicKey
  { pkParameters = dsparams
  , pkKey = convBytes . fromJust . mkHex . concat $
    [ "2d026f4bf30195ede3a088da85e398ef869611d0f68f07"
    , "13d51c9c1a3a26c95105d915e2d8cdf26d056b86b8a7b8"
    , "5519b1c23cc3ecdc6062650462e3063bd179c2a6581519"
    , "f674a61f1d89a1fff27171ebc1b93d4dc57bceb7ae2430"
    , "f98a6a4d83d8279ee65d71c1203d2c96d65ebbf7cce9d3"
    , "2971c3de5084cce04a2e147821" ]}

-- The SHA-1 hash of the matching private key.
privateKey_hash :: Bytes
privateKey_hash = toBytes . fromJust . mkHex $
  "ca8f6f7c66fa362d40760d135b763eb8527d3d52"

tests =
  [ testGroup "Challenge 44"
    [ testCase "break DSA key from repeated nonces" $ do
        msgs <- parseFile . lines <$> readFile filename
        let privateKey = fromJust $ dsaBreakRepeatedK publicKey msgs
        sha1Hash (showHex privateKey) @=? fromBytes privateKey_hash
    ]
  ]
 where
  -- Parse the data file, which is in the form
  --   msg: Message signed
  --   s: <hex value of s>
  --   r: <hex value of r>
  --   m: <sha1 hash of msg>
  --   ...
  parseFile (msgL:sL:rL:mL:xs) = ( fromJust . mkHex . drop 3 $ mL
                                 , ( read . drop 3 $ rL
                                   , read . drop 3 $ sL ) ) : parseFile xs
  parseFile _ = []
