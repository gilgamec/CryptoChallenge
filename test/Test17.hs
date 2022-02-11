-- Tests for Challenge 17.
-- Checks that we can correctly decrypt all of the mystery strings.

module Test17 ( tests ) where

import Challenge17 ( checkCBCPadding, breakCBCPadding )

import Bytes.Base64 ( Base64, mkBase64 )
import Random ( randomBytes )
import AES ( encryptCBC )
import Padding.PKCS7 ( padPKCS7 )

import Data.Maybe ( mapMaybe )

import TestFramework

mysteryStrings :: [Base64]
mysteryStrings = mapMaybe mkBase64
  [ "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc="
  , "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic="
  , "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw=="
  , "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg=="
  , "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl"
  , "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA=="
  , "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw=="
  , "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8="
  , "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g="
  , "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93"
  ]

tests =
  [ testGroup "Challenge 17" $
    flip map [1..length mysteryStrings] $
    \ix -> testCase ("correctly decrypt string #"++show ix) $ do
      key <- randomBytes 16
      iv <- randomBytes 16
      let padMsg = padPKCS7 16 $ mysteryStrings !! (ix-1)
          Just ct = encryptCBC key iv padMsg
      breakCBCPadding (uncurry $ checkCBCPadding key) (iv,ct) @?= padMsg
  ]
