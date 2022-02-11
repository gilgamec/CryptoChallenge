-- Test for Challenge 52.
-- Checks that a chained hash function is only as strong as
-- its weakest link.

module Test52 ( tests ) where

import Challenge52 ( mdChained, chainHashCollision )

import TestFramework

tests =
  [ testGroup "Challenge 52"
    [ testCase "easily break a (2+4)-byte MD hash function" $ do
        let weak = 2
            strong = 4
            hashFn = mdChained weak strong
            (x1,x2) = chainHashCollision weak strong
        hashFn x1 @?= hashFn x2
        not (x1 == x2) @? "chain hash collision between identical blocks"
    ]
  ]
