-- Test for Challenge 37.
-- Checks that we can spoof the SRP protocol from the last Challenge.

module Test37 ( tests ) where

import Bytes ( Bytes )
import CommChannel ( newChannels, runTwoBots )

import Challenge36 ( storedPassword, srpServer )
import Challenge37 ( srpClient0 )

import TestFramework

import qualified Control.Monad.Random as R

-- A list of emails and passwords.
userlist :: [(String,String)]
userlist = [ ("nobody@example.com", "superman")
           , ("somebody@foo.invalid", "passw0rd") ]

-- Create a salt-hashed-password pair.
mkPWAssoc :: (String,String) -> IO (String,(Bytes,Integer))
mkPWAssoc (u,p) = do
  bi <- storedPassword p
  pure (u,bi)

tests =
  [ testGroup "Challenge 37"
    [ repeatTestCase "SRP with evil client is successful" 10 $ do
        pwlist <- traverse mkPWAssoc userlist
        (chC,chS) <- newChannels
        -- Pick a random user from the user list to impersonate.
        unum <- R.getRandomR (0,length userlist - 1)
        let user = fst (userlist !! unum)
        (okC,okS) <- runTwoBots (srpClient0 user chC) (srpServer pwlist chS)
        (okC,okS) @?= (True,True)
    ]
  ]
