-- Tests for Challenge 38.
-- Checks that the weakened SRP protocol works for validation,
-- then checks that an evil server can reveal a weak password.

module Test38 ( tests ) where

import Bytes ( HasBytes(..), Bytes )
import Bytes.Integral ()
import CommChannel ( newChannels, runTwoBots )

import Challenge36 ( storedPassword )
import Challenge38 ( srpPrehash, srpClientWeak, srpServerWeak, srpEvilServer )

import TestFramework

import qualified Control.Monad.Random as R

-- A list of emails and passwords.
userlist :: [(String,String)]
userlist = [ ("nobody@example.com", "superman")
           , ("somebody@foo.invalid", "passw0rd")
           , ("some1@fakemail.example", "trustno1") ]

-- Create a salt-hashed-password pair.
mkPWAssoc :: (String,String) -> IO (String,(Bytes,Integer))
mkPWAssoc (u,p) = do
  bi <- storedPassword p
  pure (u,bi)

-- Pre-chosen values of u, b, and salt for the evil server.
evilU, evilB :: Integer
evilU = 12345678
evilB = 87654321

evilSalt :: Bytes
evilSalt = toBytes "YELLOW SUBMARINE"

-- Precomputed table of pre-hashed values for common passwords.
pwPrehash :: [(String,Integer)]
pwPrehash = zip pws $ map (srpPrehash evilB evilU evilSalt) pws
 where
  -- Passwords from the Wikipedia page "List of the most common passwords".
  pws =
    [ "!@#$%^&*", "000000", "111111", "121212", "123123", "1234", "12345"
    , "123456", "1234567", "12345678", "123456789", "1234567890", "1qaz2wsx"
    , "654321", "666666", "696969", "Football", "aa123456", "abc123", "access"
    , "admin", "ashley", "azerty", "bailey", "baseball", "batman", "charlie"
    , "donald", "dragon", "flower", "football", "freedom", "hello", "hottie"
    , "iloveyou", "jesus", "letmein", "login", "loveme", "master", "michael"
    , "monkey", "mustang", "ninja", "passw0rd", "password", "password1"
    , "princess", "qazwsx", "qwerty", "qwerty123", "qwertyuiop", "shadow"
    , "solo", "starwars", "sunshine", "superman", "trustno1", "welcome"
    , "whatever", "zaq1zaq1" ]

tests =
  [ testGroup "Challenge 38"
    [ repeatTestCase "Client and server acknowledge successful SRP" 10 $ do
        pwlist <- traverse mkPWAssoc userlist
        (chC,chS) <- newChannels
        -- Pick a random user from the user list.
        unum <- R.getRandomR (0,length userlist - 1)
        let user = userlist !! unum
        (okC, okS) <- runTwoBots (srpClientWeak user chC)
                                 (srpServerWeak pwlist chS)
        (okC,okS) @?= (True,True)
    , repeatTestCase "Server does not acknowledge incorrect password" 10 $ do
        pwlist <- traverse mkPWAssoc userlist
        (chC,chS) <- newChannels
        -- Pick a random user from the user list.
        unum <- R.getRandomR (0,length userlist - 1)
        let user = (fst $ userlist !! unum, "wrong_password")
        (okC,okS) <- runTwoBots (srpClientWeak user chC)
                                (srpServerWeak pwlist chS)
        okS @?= False
    , repeatTestCase "Evil server finds weak passwords" 10 $ do
        (chC,chS) <- newChannels
        -- Pick a random user from the user list.
        unum <- R.getRandomR (0,length userlist - 1)
        let user = userlist !! unum
        (okC,mep) <- runTwoBots (srpClientWeak user chC)
                       (srpEvilServer evilB evilU evilSalt pwPrehash chS)
        mep @?= Just user
    ]
  ]
