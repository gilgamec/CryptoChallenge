# Solution to Challenge 38

```haskell
module Challenge38
  (
    srpClientWeak, srpServerWeak
  , srpPrehash, srpEvilServer
  ) where

import Bytes ( HasBytes(..), convBytes, Bytes )
import CommChannel ( Channel, send, recv )
import Hash ( sha256Hash, MAC(..), mkHMACSHA256, validateHMACSHA256 )
import Modulo ( mkMod, modulo, (^%) )
import PublicKey ( KeyPair(..) )
import PublicKey.DiffieHellman ( DHParams(..), genDHKeyPair )
import Random ( randomBytes )

import Challenge36 ( srpDHParams )

import Data.Maybe ( listToMaybe )
```

This Challenge involves a weakened form of SRP.
In this weak SRP, the client and server exchange the normal
DH public keys A = g^a and B = g^b,
rather than modifying B using the hashed password.
In addition, u is now a random number
(rather than the hash of the public keys).
The client computes S = B^(a + ux) = g^ab * g^bux, and
the server computes S = (A * g^ux)^b = g^ab * g^bux,
which still agree.

### Weak client protocol

```haskell
srpClientWeak :: (String,String) -> Channel -> IO Bool
srpClientWeak (email,password) ch = do
  let DHParams p g = srpDHParams
```

Generate a key pair and send the email and the public key A to the server.

```haskell
  kp <- genDHKeyPair srpDHParams
  send ch email
  send ch (kpPublic kp)
```

Receive the salt and server's public key B.

```haskell
  salt <- recv ch
  bPublic <- fromBytes <$> recv ch
```

The random exponent u is generated by the server,
and we receive it here.

```haskell
  u <- fromBytes <$> recv ch
```

We then compute the shared secret.

```haskell
  let x = convBytes $ sha256Hash $ salt <> toBytes password :: Integer
      shared = (fromInteger bPublic ^% (kpPrivate kp + u * x)) `modulo` p
```

Using a hash of the shared secred as a shared key,
send an HMAC of the salt to validate ourselves to the server,
and validate the HMAC it sends back.

```haskell
  let key = sha256Hash shared
      hmac1 = mkHMACSHA256 key salt
  send ch (macHash hmac1)

  hmac2 <- MAC (macHash hmac1) . fromBytes <$> recv ch
  pure $ validateHMACSHA256 key hmac2
```

### Weak server protocol

The server has a list of emails and stored passwords.

```haskell
srpServerWeak :: [(String,(Bytes,Integer))] -> Channel -> IO Bool
srpServerWeak pwlist ch = do
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

Look up the stored password for that email.

```haskell
  let Just (salt,v) = lookup email pwlist
```

Send the salt and our **unmodified** public key B.

```haskell
  send ch salt
  let bPublic = kpPublic kp
  send ch bPublic
```

Generate a 128-bit random number u and send it to the client.

```haskell
  u <- fromBytes <$> randomBytes 16 :: IO Integer
  send ch u
```

Compute the shared secret.

```haskell
  let shared = ((fromInteger aPublic * mkMod v ^% u) ^% kpPrivate kp) `modulo` p
```

Using a hash of this shared secred as a shared key,
validate an HMAC of the salt to confirm the identity of the client,
and send our own HMAC to validate ourselves.

```haskell
  let key = sha256Hash shared
  hmac1 <- MAC salt . fromBytes <$> recv ch

  let hmac2 = mkHMACSHA256 key (macHash hmac1)
  send ch (macHash hmac2)

  pure $ validateHMACSHA256 key hmac1
```

### Evil server

There's a reason that SRP requires the exchanged B parameter
to depend on the password.
Suppose an adversary is posing as the server.
It can make a chosen public key and a chosen salt to send to the client,
who then computes a shared secret as usual, and sends

      hmac(K,salt)
    = hmac(SHA256(s),salt)
    = hmac(SHA256((A * g^(x*u))^b),salt)
    = hmac(SHA256(A^b * (g^x)^(ub)),salt)
    = hmac(SHA256(A^b * g^(SHA256(salt|password)*u*b)),salt)
    = hmac(SHA256(A^b * f(b,u,salt,password)),salt)

Since the evil server picks the private key b, salt, and u,
it can just precompute a big table of f(b,u,salt,password)
and figure out the password from the HMAC!

The function f is computed by the function `srpPrehash`.

```haskell
srpPrehash :: Integer -> Integer -> Bytes -> String -> Integer
srpPrehash b u salt pw =
  let DHParams p g = srpDHParams
      exponent = u * b * convBytes (sha256Hash $ salt <> toBytes pw)
  in  (mkMod g ^% exponent) `modulo` p
```

The impostor server is started with the values of b, u, and the salt,
as well as a table of the values of `f` for a bunch of passwords.

```haskell
srpEvilServer :: Integer -> Integer -> Bytes -> [(String,Integer)]
              -> Channel -> IO (Maybe (String,String))
srpEvilServer b u salt pws ch = do
  let DHParams p g = srpDHParams
```

Act like the normal server,
receiving the client's email and public key.

```haskell
  email <- fromBytes <$> recv ch
  aPublic <- fromBytes <$> recv ch
```

Generate a fake public key with the given private key.

```haskell
  let publicKey = (fromInteger g ^% b) `modulo` p
```

Send the salt, public key, and u.

```haskell
  send ch salt
  send ch publicKey
  send ch u
```

Receive an HMAC of the salt.

```haskell
  hmac1 <- MAC salt . fromBytes <$> recv ch
```

Now we have what we need!
For every entry in the prehashed password list,
we compute the key generated using it
and use that validate the HMAC.
Only the correct password will validate correctly!

```haskell
  let gab = (fromInteger aPublic ^% b) `modulo` p
      mbPassword = listToMaybe
        [ pw | (pw,f) <- pws
             , let key = sha256Hash $ (gab * f) `mod` p
             , validateHMACSHA256 key hmac1 ]
```

We complete the protocol, sending whatever back to the client,
and return the (email,password) pair we found.

```haskell
  send ch "pwned"
  pure $ (,) <$> Just email <*> mbPassword
```