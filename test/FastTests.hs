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
import qualified Test10
import qualified Test11
import qualified Test12
import qualified Test13
import qualified Test14
import qualified Test15
import qualified Test16

import qualified Test17
import qualified Test18
-- no Test19
import qualified Test20
import qualified Test21
import qualified Test22
import qualified Test23
import qualified Test24

import qualified Test25
import qualified Test26

main = defaultMain $
  [ testGroup "Set 1" $ concat [ Test1.tests, Test2.tests, Test3.tests
                               , Test4.tests, Test5.tests, Test6.tests
                               , Test7.tests, Test8.tests ]
  , testGroup "Set 2" $ concat [ Test9.tests, Test10.tests, Test11.tests
                               , Test12.tests, Test13.tests, Test14.tests
                               , Test15.tests, Test16.tests ]
  , testGroup "Set 3" $ concat [ Test17.tests, Test18.tests, Test20.tests
                               , Test21.tests, Test22.tests, Test23.tests
                               , Test24.tests ]
  , testGroup "Set 4" $ concat [ Test25.tests, Test26.tests ]
  ]
