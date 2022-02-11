# Solution to Challenge 37

```haskell
module Challenge37
  (
    srpDHParams
  , srpClient0
  ) where

import Bytes ( HasBytes(..) )
import CommChannel ( Channel, send, recv )
import Hash ( sha256Hash, MAC(..), mkHMACSHA256, validateHMACSHA256  )
import PublicKey.DiffieHellman ( DHParams(..) )

import Challenge36 ( srpDHParams )

import qualified Control.Monad.Random as R
```

If the SRP client sends 0 as its public key,
then the server will compute

    S = (0 * (g^x)^u) ^ b = 0.
	
We can then use S as the shared key.

Our evil client only needs an email address.

```haskell
srpClient0 :: String -> Channel -> IO Bool
srpClient0 email ch = do
  let DHParams p g = srpDHParams
```

Don't bother with the key pair.
Send the email and a random multiple of p.

```haskell
  send ch email
  n <- R.getRandomR (1,10)
  send ch (n * p)
```

Receive the salt and throw away the modified public key B.

```haskell
  salt <- recv ch
  recv ch
```

The shared secret is just 0.
Use it as the key for an HMAC of the salt
to validate ourselves to the server.

```haskell
  let shared = 0 :: Integer
      key = sha256Hash shared

  let hmac1 = mkHMACSHA256 key salt
  send ch (macHash hmac1)
```

Receive and validate an HMAC of the previous HMAC.

```haskell
  hmac2 <- MAC (macHash hmac1) . fromBytes <$> recv ch
  pure $ validateHMACSHA256 key hmac2
```
