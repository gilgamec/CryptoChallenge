-- Tests for Challenge 35.
-- Checks message interception by sending 1, p, or p-1 in place of g.

module Test35 ( tests ) where

import CommChannel ( mitmChannels, runThreeBots )
import Random ( randomBytesR )

import Challenge34 ( dhBotA, dhBotB )
import Challenge35 ( dhMITM_1, dhMITM_p, dhMITM_p1 )

import TestFramework

tests =
  [ testGroup "Challenge 35"
    [ repeatTestCase "g = 1: eavesdropper receives A's message" 10 $ do
        messageA <- randomBytesR (40,60)
        messageB <- randomBytesR (40,60)
        ((ma,mb),(am,bm)) <- mitmChannels
        (recvA, recvB, mitmA) <- runThreeBots
          (dhBotA messageA ma) (dhBotB messageB mb) (dhMITM_1 am bm)
        mitmA @?= messageA
    , repeatTestCase "g = p: eavesdropper receives A's message" 10 $ do
        messageA <- randomBytesR (40,60)
        messageB <- randomBytesR (40,60)
        ((ma,mb),(am,bm)) <- mitmChannels
        (recvA, recvB, mitmA) <- runThreeBots
          (dhBotA messageA ma) (dhBotB messageB mb) (dhMITM_p am bm)
        mitmA @?= messageA
    , repeatTestCase "g = p-1: eavesdropper receives A's message" 10 $ do
        messageA <- randomBytesR (40,60)
        messageB <- randomBytesR (40,60)
        ((ma,mb),(am,bm)) <- mitmChannels
        (recvA, recvB, mitmA) <- runThreeBots
          (dhBotA messageA ma) (dhBotB messageB mb) (dhMITM_p1 am bm)
        messageA `elem` mitmA @? "did not get A's message"
    ]
  ]
