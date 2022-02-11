-- Test driver for 'fast' tests;
-- that is, tests which each complete quickly (in a few seconds).

module FastTests where

import Test.Framework ( defaultMain, testGroup )

import qualified Test1
import qualified Test2
import qualified Test3
import qualified Test4
import qualified Test5
import qualified Test6
import qualified Test7
import qualified Test8

import qualified Test9

main = defaultMain $
  [ testGroup "Set 1" $ concat [ Test1.tests, Test2.tests, Test3.tests
                               , Test4.tests, Test5.tests, Test6.tests
                               , Test7.tests, Test8.tests ]
  , testGroup "Set 2" $ concat [ Test9.tests ]
  ]
