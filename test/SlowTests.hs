-- Test driver for 'slow' tests;
-- that is, tests which take more than a few seconds to complete.

module SlowTests where

import Test.Framework ( defaultMain, testGroup )

import qualified Test56

main = defaultMain $
  [ testGroup "Set 7" $ Test56.tests ]
