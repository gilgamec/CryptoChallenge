-- Tests for Challenge 36.
-- Checks that the SRP implementation works correctly.

module Test36 ( tests ) where

import Bytes ( Bytes )
import CommChannel ( newChannels, runTwoBots )

import Challenge36 ( srpClient, srpServer, storedPassword )

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
  [ testGroup "Challenge 36"
    [ repeatTestCase "Client and server acknowledge successful SRP" 10 $ do
        pwlist <- traverse mkPWAssoc userlist
        (chC,chS) <- newChannels
        -- Pick a random user from the user list.
        unum <- R.getRandomR (0,length userlist - 1)
        let user = userlist !! unum
        (okC, okS) <- runTwoBots (srpClient user chC) (srpServer pwlist chS)
        (okC,okS) @?= (True,True)
    , repeatTestCase "Server does not acknowledge incorrect password" 10 $ do
        pwlist <- traverse mkPWAssoc userlist
        (chC,chS) <- newChannels
        -- Pick a random user from the user list.
        unum <- R.getRandomR (0,length userlist - 1)
        let user = (fst $ userlist !! unum, "wrong_password")
        (okC,okS) <- runTwoBots (srpClient user chC) (srpServer pwlist chS)
        okS @?= False
    ]
  ]
