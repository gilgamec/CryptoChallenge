-- Test driver for 'slow' tests;
-- that is, tests which take more than a few seconds to complete.

module SlowTests where

import Test.Framework ( defaultMain, testGroup )

import qualified Test56

import qualified Test60
import qualified Test64
import qualified Test65

main = defaultMain $
  [ testGroup "Set 7" $ Test56.tests
  , testGroup "Set 8" $ concat [ Test60.testsSlow
                               , Test64.tests
                               , Test65.tests ] ]
