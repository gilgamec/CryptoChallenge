-- Test driver for 'fast' tests;
-- that is, tests which each complete quickly (in a few seconds).

module FastTests where

import Test.Framework ( defaultMain )

import qualified Test1
import qualified Test2

main = defaultMain $ concat [ Test1.tests, Test2.tests ]
