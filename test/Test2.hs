-- Test for Challenge 2.
-- Checks that the given strings of bytes really satisfy
--   hex1 `xor` hex2 == hex3

module Test2 ( tests ) where

import Bytes ( HasBytes(..), xorb )
import Bytes.Hex ( Hex, mkHex )

import Data.Maybe ( fromJust )

import TestFramework

-- Three hex-encoded sequences of bytes.
hex1, hex2, hex3 :: Hex
hex1 = fromJust $ mkHex "1c0111001f010100061a024b53535009181c"
hex2 = fromJust $ mkHex "686974207468652062756c6c277320657965"
hex3 = fromJust $ mkHex "746865206b696420646f6e277420706c6179"


tests =
  [ testGroup "Challenge 2" $
    [ testCase "xorb gives correct result" $
        (hex1 `xorb` hex2) @?= toBytes hex3 ] ]
