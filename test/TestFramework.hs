-- Test framework for CryptoChallange
-- Largely a re-export of functionality from
-- test-framework and HUnit.

module TestFramework
  (
    testGroup, testCase, repeatTestCase
  , module Test.HUnit
  ) where

import Control.Monad ( replicateM_ )

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

-- Repeat a test case a number of times.
-- Useful if some test parameters are randomly generated.
repeatTestCase :: TestName -> Int -> Assertion -> Test.Framework.Test
repeatTestCase name n = testCase (name ++ " (x"++show n++")") . replicateM_ n
