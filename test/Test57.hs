-- Test for Challenge 57.
-- Checks that we can use Pohlig-Hellman to reassemble the private key
-- by confining the secret to several small subgroups.

module Test57 ( tests ) where

import PublicKey ( KeyPair(..), publicPart )
import PublicKey.DiffieHellman ( DHParams(..) )

import Challenge57 ( genDHKeyPair', bobEncoder, getPrivateKey )

import TestFramework

-- The DH parameters from the Challenge text
params :: DHParams
params = DHParams
  { dhModulus = 7199773997391911030609999317773941274322764333428698921736339643928346453700085358802973900485592910475480089726140708102474957429903531369589969318716771
  , dhGenerator = 4565356397095740655436854503483826832136106141639563487732438195343690437606117828318042418238184896212352329118608100083187535033402010599512641674644143 }

setQ :: Integer
setQ = 236234353446506858198510045061214171961

-- The message to sign
message :: String
message = "crazy flamboyant for the rap enjoyment"

tests =
  [ testGroup "Challenge 57"
    [ repeatTestCase "reassemble Bob's private key" 2 $ do
        bobKey <- genDHKeyPair' params setQ
        let oracle = bobEncoder bobKey message
        prv <- getPrivateKey oracle (publicPart bobKey) setQ
        prv @?= kpPrivate bobKey
    ]
  ]
