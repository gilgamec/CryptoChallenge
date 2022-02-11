# Solution to Challenge 35

```haskell
module Challenge35
  (
    dhMITM_1, dhMITM_p, dhMITM_p1 
  ) where

import Bytes ( HasBytes(..), Bytes )
import AES ( decryptCBC )
import CommChannel ( Channel, send, recv )
import Hash ( sha1Hash )
import Padding.PKCS7 ( validatePKCS7 )

import Data.Maybe ( fromJust, catMaybes )

import qualified Data.ByteString as B
```

The clients for this one are the same as for the last Challenge.
The eavesdropper meddles with `g` this time.
Since `g` doesn't affect B's private key, which is used to
generate B's version of the shared secret,
we are in no case able to decrypt B's message.
A's message, however, will be entirely decryptable.

All three bots share the same protocol,
so we just combine them into one
which takes two functions:
one for computing `g` from `p`,
and one for decrypting A's message given its IV and ciphertext,
along with B's public key.

```haskell
dhMITM_g :: (Integer -> Integer) -> (Integer -> Bytes -> Bytes -> a)
         -> Channel -> Channel -> IO a
dhMITM_g calcG decrypt chA chB = do
```

We read and relay `p` and `g`, modifying the latter.

```haskell
  p <- fromBytes <$> recv chA
  send chB p

  recv chA
  let g = calcG p
  send chB g
```

Relay the public keys.

```haskell
  recv chA >>= send chB
  bPublic <- fromBytes <$> recv chB
  send chA bPublic
```

Receive and relay A's ciphertext and IV.

```haskell
  ctA <- recv chA
  send chB ctA
  ivA <- recv chA
  send chB ivA
```

Relay B's ciphertext and IV.

```haskell
  recv chB >>= send chA
  recv chB >>= send chA
```

Now, call the `decrypt` function to find A's message.

```haskell
  pure $ decrypt bPublic ctA ivA
```

### Case 1: g = 1

If we set `g` to 1, then B's public key is 1, so A thinks the shared secret is 1.

```haskell
dhMITM_1 :: Channel -> Channel -> IO Bytes
dhMITM_1 =
  let key = B.take 16 $ toBytes $ sha1Hash (1 :: Integer)
      decrypt _ ct iv = fromJust $ decryptCBC key iv ct >>= validatePKCS7
  in  dhMITM_g (const 1) decrypt
```

### Case 2: g = p

If we set `g` to `p`, then B's public key is zero and A's shared secret is zero.

```haskell
dhMITM_p :: Channel -> Channel -> IO Bytes
dhMITM_p =
  let key = B.take 16 $ toBytes $ sha1Hash (0 :: Integer)
      decrypt _ ct iv = fromJust $ decryptCBC key iv ct >>= validatePKCS7
  in  dhMITM_g id decrypt
```

### Case 3: g = p-1

Finally, if `g = p-1`, then `g^2 = 1`;
thus, B's public key will be either 1 (if his private key is even)
or p-1 (if his private key is odd).
In the former case, the shared secret computed by A will be 1.
In the latter case, the shared secret will be either 1 or p-1
(depending on whether A's private key is odd).
In other words, the secret will be either 1 or `b`.

```haskell
dhMITM_p1 :: Channel -> Channel -> IO [Bytes]
dhMITM_p1 =
  let decrypt b ct iv =
        let key1 = B.take 16 $ toBytes $ sha1Hash (1 :: Integer)
            keyb = B.take 16 $ toBytes $ sha1Hash b
        in  catMaybes [ decryptCBC key1 iv ct >>= validatePKCS7
                      , decryptCBC keyb iv ct >>= validatePKCS7 ]
  in  dhMITM_g (subtract 1) decrypt
```
