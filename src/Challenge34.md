# Solution to Challenge 34

```haskell
module Challenge34
  (
    dhBotA, dhBotB, dhMITMBot
  ) where

import Bytes ( HasBytes(..), Bytes )
import AES ( encryptCBC, decryptCBC )
import CommChannel ( Channel, send, recv )
import Hash ( sha1Hash )
import Padding.PKCS7 ( padPKCS7, validatePKCS7 )
import PublicKey ( KeyPair(..), PublicKey(..) )
import PublicKey.DiffieHellman ( DHParams(..), genDHKeyPair, dhSharedSecret
                               , prime1536 )
import Random ( randomBytes )

import qualified Data.ByteString as B
```

We use the stuff from CommChannel to implement the protocol
described in the Challenge.
First, participant A.

```haskell
dhBotA :: Bytes -> Channel -> IO Bytes
dhBotA message ch = do
```

> Send "p", "g", "A"

```haskell
  let params@(DHParams p g) = DHParams prime1536 2
  send ch p
  send ch g

  aKeyPair <- genDHKeyPair params
  send ch $ kpPublic aKeyPair
```

> (Receive) "B"

```haskell
  bPublic <- PublicKey params . fromBytes <$> recv ch
```

> Send AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv

```haskell
  let shared = dhSharedSecret aKeyPair bPublic
      key = B.take 16 $ toBytes $ sha1Hash shared
  iv1 <- randomBytes 16
  let Just ct1 = encryptCBC key iv1 $ padPKCS7 16 message
  send ch ct1
  send ch iv1
```

> (Receive) AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv

```haskell
  ct2 <- recv ch
  iv2 <- recv ch
  let Just message2 = decryptCBC key iv2 ct2 >>= validatePKCS7
  pure message2
```

Then participant B.

```haskell
dhBotB :: Bytes -> Channel -> IO Bytes
dhBotB message ch = do
```

> (Receive) "p", "g", "A"

```haskell
  params <- DHParams <$> (fromBytes <$> recv ch) <*> (fromBytes <$> recv ch)
  aPublic <- PublicKey params . fromBytes <$> recv ch
```

> Send "B"

```haskell
  bKeyPair <- genDHKeyPair params
  send ch $ kpPublic bKeyPair
```

> (Receive) AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv

```haskell
  let shared = dhSharedSecret bKeyPair aPublic
      key = B.take 16 $ toBytes $ sha1Hash shared
  ct1 <- recv ch
  iv1 <- recv ch
  let Just message2 = decryptCBC key iv1 ct1 >>= validatePKCS7
```

> Send AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv

```haskell
  iv2 <- randomBytes 16
  let Just ct2 = encryptCBC key iv2 $ padPKCS7 16 message
  send ch ct2
  send ch iv2

  pure message2
```

And the eavesdropper protocol:

```haskell
dhMITMBot :: Channel -> Channel -> IO (Bytes,Bytes)
dhMITMBot chA chB = do
```

> (Receive from A) "p", "g", "A"

```haskell
  p <- recv chA
  g <- recv chA
  recv chA
```

> (Send to B) "p", "g", "p"

```haskell
  send chB p
  send chB g
  send chB p
```

> (Receive from B) "B"

```haskell
  recv chB
```

> (Send to A) "p"

```haskell
  send chA p
```

> (Receive from A) AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv
> Relay that to B

```haskell
  ctA <- recv chA
  send chB ctA
  ivA <- recv chA
  send chB ivA
```

> (Receive from B) AES-CBC(SHA1(s)[0:16], iv=random(16), msg) + iv
> Relay that to A

```haskell
  ctB <- recv chB
  send chA ctB
  ivB <- recv chB
  send chA ivB
```

What we've done is told A that B's public key is `p`, and vice versa.
This means that A thinks the shared secret is

    p ^ (A's private key) = 0 mod p

and similarly for B.
We can thus trivially decrypt the messages.

```haskell
  let shared = toBytes (0 :: Integer)
      key = B.take 16 $ toBytes $ sha1Hash shared
      Just messageA = decryptCBC key ivA ctA >>= validatePKCS7
      Just messageB = decryptCBC key ivB ctB >>= validatePKCS7

  pure (messageA,messageB)
```
