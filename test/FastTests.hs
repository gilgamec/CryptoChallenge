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
import qualified Test27
import qualified Test28
import qualified Test29
import qualified Test30
-- Tests 31 and 32 are not fast

import qualified Test33
import qualified Test34
import qualified Test35
import qualified Test36
import qualified Test37
import qualified Test38
import qualified Test39
import qualified Test40

import qualified Test41
import qualified Test42
import qualified Test43
import qualified Test44
import qualified Test45
import qualified Test46
-- Tests 47 and 48 are not fast

import qualified Test49
import qualified Test50
import qualified Test51
import qualified Test52
import qualified Test53
import qualified Test54
import qualified Test55
-- Test 56 is not fast

import qualified Test57
import qualified Test58
import qualified Test59
import qualified Test60
import qualified Test61
import qualified Test62

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
  , testGroup "Set 4" $ concat [ Test25.tests, Test26.tests, Test27.tests
                               , Test28.tests, Test29.tests, Test30.tests ]
  , testGroup "Set 5" $ concat [ Test33.tests, Test34.tests, Test35.tests
                               , Test36.tests, Test37.tests, Test38.tests
                               , Test39.tests, Test40.tests ]
  , testGroup "Set 6" $ concat [ Test41.tests, Test42.tests, Test43.tests
                               , Test44.tests, Test45.tests, Test46.tests ]
  , testGroup "Set 7" $ concat [ Test49.tests, Test50.tests, Test51.tests
                               , Test52.tests, Test53.tests, Test54.tests
                               , Test55.tests ]
  , testGroup "Set 8" $ concat [ Test57.tests, Test58.tests, Test59.tests
                               , Test60.testsFast, Test61.tests
                               , Test62.tests ]
  ]
