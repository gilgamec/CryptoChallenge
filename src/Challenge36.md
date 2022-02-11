# Solution to Challenge 36

This module implements an SRP client and server.

```haskell
module Challenge36
  (
    srpDHParams, srpKParam
  , storedPassword
  , srpClient, srpServer
  ) where

import Bytes ( HasBytes(..), Bytes, convBytes )
import CommChannel ( Channel, send, recv )
import Hash ( sha256Hash, MAC(..), mkHMACSHA256, validateHMACSHA256 )
import Modulo ( mkMod, modulo, (^%) )
import PublicKey ( KeyPair(..) )
import PublicKey.DiffieHellman ( DHParams(..), genDHKeyPair, prime1536 )
import Random ( randomBytes )

import qualified Control.Monad.Random as R
```

Mathematically, the SRP protocol works as follows:

1. The server has stored the password as (salt, v = g^(H(salt|password))).
   (The hashed password, x = H(salt|password), is never stored on the server.)
2. The server sends the salt to the client, who computes x = H(salt|password).
3. The client sends a normal DH public key A = g^a.
4. The server sents B = k*v + g^b.
5. Both client and server compute u = H(A|B).
6. The client computes
    S = (B - k * (g^x)) ^ (a + u * x)
      = (g^b) ^ (a + u*x)
      = (g^ab) (g^uxb)
7. The server computes
    S = (A * v^u) ^ b
      = (g^a * g^ux) ^ b
      = (g^ab) (g^uxb).
8. Client and server now have a shared secret; they exchange HMACs of
   known information (like the public keys or the salt) to confirm this.

The parameters are agreed upon beforehand:
here the prime is the 1536-bit NIST prime, g is 2, and k is 3.

```haskell
srpDHParams :: DHParams
srpDHParams = DHParams{ dhModulus = prime1536, dhGenerator = 2 }

srpKParam :: Integer
srpKParam = 3
```

## Client protocol

The client wants to confirm its email and password with the server.

```haskell
srpClient :: (String,String) -> Channel -> IO Bool
srpClient (email,password) ch = do
  let DHParams p g = srpDHParams
```

Generate a key pair and send the email and the public key A to the server.

```haskell
  kp <- genDHKeyPair srpDHParams
  send ch email
  send ch (kpPublic kp)
```

Receive the salt and modified public key B.

```haskell
  salt <- recv ch
  bPublic <- fromBytes <$> recv ch
```

We need the hash of the salt and password,
and the hash of the two public keys.

```haskell
  let x = convBytes $ sha256Hash $ salt <> toBytes password
      u = convBytes $ sha256Hash $ toBytes (kpPublic kp) <> toBytes bPublic
```

We then compute the shared secret, modulo the prime.

```haskell
  let shared = ((fromInteger bPublic - mkMod srpKParam * mkMod g ^% x) ^%
                (kpPrivate kp + u * x)) `modulo` p
```

We use a hash of the shared secred as a shared key.
Send an HMAC of the salt to validate ourselves to the server.


```haskell
  let key = sha256Hash shared
      hmac1 = mkHMACSHA256 key salt
  send ch (macHash hmac1)
```

The server sends an HMAC of the previous hash to validate itself to us.

```haskell
  hmac2 <- MAC (macHash hmac1) . fromBytes <$> recv ch
  pure $ validateHMACSHA256 key hmac2
```

## Server protocol

The server has a list of emails and stored passwords.

```haskell
srpServer :: [(String,(Bytes,Integer))] -> Channel -> IO Bool
srpServer pwlist ch = do
  let DHParams p g = srpDHParams
```

Generate a key pair from the agreed-upon parameters.

```haskell
  kp <- genDHKeyPair srpDHParams
```

Receive the client's email and public key A.

```haskell
  email <- fromBytes <$> recv ch
  aPublic <- fromBytes <$> recv ch
```

Look up the stored password, which is a salt and a number
v = g^(H(salt|password)).
This toy implementation fails if the email has no associated password.

```haskell
  let Just (salt,v) = lookup email pwlist
```

Send the salt and modified public key B.

```haskell
  send ch salt
  let bPublic = (fromInteger srpKParam * fromInteger v +
                 fromInteger (kpPublic kp)) `modulo` p
  send ch bPublic
```

Compute the shared secret from the hash of the two public keys.

```haskell
  let u = convBytes $ sha256Hash $ toBytes aPublic <> toBytes bPublic :: Integer
      shared = ((fromInteger aPublic * mkMod v ^% u) ^% kpPrivate kp) `modulo` p
```

Using a hash of this shared secred as a shared key,
we validate an HMAC of the salt to confirm the identity of the client.

```haskell
  let key = sha256Hash shared
  hmac1 <- MAC salt . fromBytes <$> recv ch
  let isValidClient = validateHMACSHA256 key hmac1
```

Finally, we send an HMAC of the previous hash to validate us to them.

```haskell
  let hmac2 = mkHMACSHA256 key (macHash hmac1)
  send ch (macHash hmac2)
  pure isValidClient
```

The helper function `storedPassword` generates a salt
and hashes a password to create an entry for the server's password table.

```haskell
storedPassword :: R.MonadRandom m => String -> m (Bytes,Integer)
storedPassword password = do
  let DHParams p g = srpDHParams
```

The salt is just 16 random bytes.

```haskell
  salt <- randomBytes 16
```

We hash it with the password, then create the value v = g^x.

```haskell
  let x = convBytes $ sha256Hash $ salt <> toBytes password :: Integer
      v = (fromInteger g ^% x) `modulo` p
```

The stored password is then the pair of the salt and the value v.

```haskell
  pure (salt,v)
```
