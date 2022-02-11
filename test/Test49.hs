-- Tests for Challenge 49.
-- For the first (really broken) CBC-MAC, check that it works,
-- that it can be used for client-server authentication,
-- and that it is trivially breakable.
-- For the second (merely broken) CBC-MAC, check that it works,
-- can be used for authentication, and that it is also breakable.

module Test49 ( tests ) where

import Bytes ( HasBytes(..) )
import Random ( randomBytes, randomBytesR )

import Challenge49 ( Transaction1(..), mkBrokenCBCMAC, validateBrokenCBCMAC
                   , bankServer1, bankClient1, breakBank1
                   , Transaction2(..), mkCBCMAC, validateCBCMAC
                   , bankServer2, bankClient2, breakBank2 )

import Control.Monad.Random ( replicateM )
import Data.Maybe ( fromJust, isJust )

import qualified Control.Monad.Random as R

import TestFramework

-- Report whether a Transaction2 has a ("me","1000000") transaction.
megabuxToMe :: Transaction2 -> Bool
megabuxToMe Transaction2{ tx2Transactions = txs }
  = lookup "me" txs == Just "1000000"

-- Create a random Transaction2 from the given account.
randomTx2 :: R.MonadRandom m => String -> m Transaction2
randomTx2 from = do
  let tos = [ "mom", "dad", "brother", "sister"
            , "landlord", "friend", "restaurant" ]
  toS <- (tos !!) <$> R.getRandomR (0,length tos - 1)
  amt <- R.getRandomR (10,500 :: Int)
  pure Transaction2{ tx2From = from
                   , tx2Transactions = [(toS,show amt)] }

tests =
  [ testGroup "Challenge 49"
    [ testGroup "Part 1"
      [ testCase "BrokenCBCMAC validation" $ do
          key <- randomBytes 16
          iv <- randomBytes 16
          msg <- randomBytesR (40,80)
          let mac = mkBrokenCBCMAC key iv msg
          validateBrokenCBCMAC key mac @?= True
      , repeatTestCase "invalid user fails" 10 $ do
          key <- randomBytes 16
          let server = bankServer1 key
              client = bankClient1 "me" key
              evilTx = Transaction1{ tx1From = "victim"
                                   , tx1To = "me"
                                   , tx1Amt = "1000000" }
          client evilTx @?= Nothing
      , repeatTestCase "client / server" 10 $ do
          key <- randomBytes 16
          let server = bankServer1 key
              client = bankClient1 "me" key
              goodTx = Transaction1{ tx1From = "me"
                                   , tx1To = "landlord"
                                   , tx1Amt = "500" }
          (client goodTx >>= server) @?= Just goodTx
      , repeatTestCase "protocol broken" 10 $ do
          key <- randomBytes 16
          let server = bankServer1 key
              client = bankClient1 "fakeme" key
              evilMAC = breakBank1 client
          server evilMAC @?= Just Transaction1{ tx1From = "victim"
                                              , tx1To = "me"
                                              , tx1Amt = "1000000" }
      ]
    , testGroup "Part 2"
      [ testCase "CBCMAC validation" $ do
          key <- randomBytes 16
          iv <- randomBytes 16
          msg <- randomBytesR (40,80)
          let mac = mkCBCMAC key iv msg
          validateCBCMAC key iv mac @?= True
      , repeatTestCase "client / server" 10 $ do
          key <- randomBytes 16
          let server = bankServer2 key
              client = bankClient2 "me" key
              goodTx = Transaction2{ tx2From = "me"
                                   , tx2Transactions = [("landlord","500")
                                                       ,("isp","150")] }
          (client goodTx >>= server) @?= Just goodTx
      , repeatTestCase "protocol broken" 100 $ do
          key <- randomBytes 16
          let server = bankServer2 key
              client = bankClient2 "fakeme" key
              victimClient = bankClient2 "victim" key
          victimMACs <- replicateM 10 $ fromJust . victimClient <$> randomTx2 "victim"
          let evilMAC = breakBank2 client victimMACs
          isJust (server evilMAC) @?= True
          let Just evilTx = server evilMAC
          tx2From evilTx @?= "victim"
          megabuxToMe evilTx @?= True
      ]
    ]
  ]
