-- Tests for Challenge 34.
-- Checks that the protocol works correctly as implemented;
-- then check that the MITM parameter injection breaks it as expected.

module Test34 ( tests ) where

import CommChannel ( newChannels, runTwoBots, mitmChannels, runThreeBots)
import Random ( randomBytesR )

import Challenge34 ( dhBotA, dhBotB, dhMITMBot )

import TestFramework

tests =
  [ testGroup "Challenge 34"
    [ repeatTestCase "AB protocol works correctly" 10 $ do
        messageA <- randomBytesR (40,60)
        messageB <- randomBytesR (40,60)
        (chA,chB) <- newChannels
        (recvA, recvB) <- runTwoBots (dhBotA messageA chA) (dhBotB messageB chB)
        recvB @?= messageA
        recvA @?= messageB
    , repeatTestCase "MITM silently reads both messages" 10 $ do
        messageA <- randomBytesR (40,60)
        messageB <- randomBytesR (40,60)
        ((ma,mb),(am,bm)) <- mitmChannels
        (recvA, recvB, (mitmA,mitmB)) <- runThreeBots
          (dhBotA messageA ma) (dhBotB messageB mb) (dhMITMBot am bm)
        recvB @?= messageA
        recvA @?= messageB
        mitmA @?= messageA
        mitmB @?= messageB
    ]
  ]
