# Tools for handling hash functions and MACs

Quite a few Challenges deal with hash functions
and message authentication codes based on them.
This module contains some machinery for dealing with them.

```haskell
module Hash
  (
    MAC(..), MACGenerator
  , secretPrefixMAC, validateMAC
  , mkHMAC

  , SHA1Digest(..), sha1Hash
  , SHA1MAC, mkSHA1MAC, validateSHA1MAC
  , mkHMACSHA1, validateHMACSHA1

  , MD4Digest(..), md4Hash
  , MD4MAC, mkMD4MAC, validateMD4MAC
  ) where

import Bytes ( HasBytes(..), Bytes, xorb )

import Data.Maybe ( fromJust )

import qualified Crypto.Hash as H
import qualified Data.ByteArray as A
import qualified Data.ByteString as B
```

## MACs

We'll be making lots of different kinds of MACs,
so we have a general framework for them.
A MAC consists of a message (an arbitrary `HasBytes`)
and an authenticating hash of some kind
(another `HasBytes`, usually a hash digest).

```haskell
data MAC text digest = MAC{ macMessage :: text, macHash :: digest }
  deriving (Eq,Ord,Show)
```

Generating a MAC is a general operation which takes a key and a message
and produces the MAC authenticating the message with the key.

```haskell
type MACGenerator key text digest = key -> text -> MAC text digest
```

A secret-prefix MAC simply appends the text to the key
then hashes with some hash function to create the digest.

```haskell
secretPrefixMAC :: (HasBytes key, HasBytes text)
                => (Bytes -> digest) -> MACGenerator key text digest
secretPrefixMAC hash key text =
  let keytext = toBytes key <> toBytes text
  in  MAC{ macMessage = text, macHash = hash keytext }
```

Validating a MAC is a general operation, given a MAC generating function.
The validation simply regenerates the MAC with our key
and compares the generated hash with the provided one.

```haskell
validateMAC :: Eq digest
            => MACGenerator key text digest
            -> key -> MAC text digest -> Bool
validateMAC mkMAC key mac =
  let newMAC = mkMAC key (macMessage mac)
  in  macHash mac == macHash newMAC
```

### HMAC

HMAC is a generally useful MAC format which uses two layers of hashing.
It can be used with any hash function,
so we provide a general function for making an HMAC from a text.
The hash function itself and its block size must be provided.

```haskell
mkHMAC :: (HasBytes digest, HasBytes key, HasBytes text)
       => (Bytes -> digest) -> Int -> MACGenerator key text digest
mkHMAC hash blockSize key text =
```

Our key must be made precisely `blockSize` bytes long.
We right-pad it if too short, or hash it if too long.

```haskell
  let key' | numBytes key > blockSize = pad $ hash $ toBytes key
           | otherwise = pad key
      pad k = toBytes k <> B.replicate (blockSize - numBytes k) 0
```

The key is bit-flipped for use in the MAC.
In the inner hash layer, we flip bits in the pattern 00110110 (0x36);
in the outer layer, in the pattern 01011010 (0x5c).

```haskell
      ikey = key' `xorb` B.replicate blockSize 0x36
      okey = key' `xorb` B.replicate blockSize 0x5c
```

The inner hash prefixes the text with `ikey`:

```haskell
      h1 = hash (ikey <> toBytes text)
```

while the outer hash prefixes `h1` with `okey`:

```haskell
      h2 = hash (okey <> toBytes h1)
```

`h2` is the hash of the entire message.

```haskell
  in  MAC{ macMessage = text, macHash = h2 }
```

## Hash functions

We use [cryptonite](https://hackage.haskell.org/package/cryptonite)'s
implementations of hash functions.

### SHA-1

cryptonite's SHA-1 digest is a `ByteArray`;
fortunately, these can be converted to and from `ByteString`s.

```haskell
newtype SHA1Digest = SHA1Digest (H.Digest H.SHA1)
  deriving (Eq,Ord,Show)

instance HasBytes SHA1Digest where
  toBytes (SHA1Digest ba) = A.convert ba
  fromBytes = SHA1Digest . fromJust . H.digestFromByteString
  numBytes _ = H.hashDigestSize H.SHA1
```

`sha1Hash` hashes bytes into a digest.

```haskell
sha1Hash :: HasBytes text => text -> SHA1Digest
sha1Hash = SHA1Digest . H.hash . toBytes
```

We thus have trivial implementations of MAC generation and validation functions.

```haskell
type SHA1MAC text = MAC text SHA1Digest

mkSHA1MAC :: (HasBytes key, HasBytes text) => MACGenerator key text SHA1Digest
mkSHA1MAC = secretPrefixMAC sha1Hash

validateSHA1MAC :: (HasBytes key, HasBytes text) => key -> SHA1MAC text -> Bool
validateSHA1MAC = validateMAC mkSHA1MAC

mkHMACSHA1 :: (HasBytes key, HasBytes text) => MACGenerator key text SHA1Digest
mkHMACSHA1 = mkHMAC sha1Hash (H.hashBlockSize H.SHA1)

validateHMACSHA1 :: (HasBytes key, HasBytes text) => key -> SHA1MAC text -> Bool
validateHMACSHA1 = validateMAC mkHMACSHA1
```

### MD-4

The MD-4 hash implementation works exactly the same.

```haskell
newtype MD4Digest = MD4Digest (H.Digest H.MD4)
  deriving (Eq,Ord,Show)

instance HasBytes MD4Digest where
  toBytes (MD4Digest ba) = A.convert ba
  fromBytes = MD4Digest . fromJust . H.digestFromByteString
  numBytes _ = H.hashDigestSize H.MD4

md4Hash :: HasBytes text => text -> MD4Digest
md4Hash = MD4Digest . H.hash . toBytes

type MD4MAC text = MAC text MD4Digest

mkMD4MAC :: (HasBytes key, HasBytes text) => MACGenerator key text MD4Digest
mkMD4MAC = secretPrefixMAC md4Hash

validateMD4MAC :: (HasBytes key, HasBytes text) => key -> MD4MAC text -> Bool
validateMD4MAC = validateMAC mkMD4MAC
```
