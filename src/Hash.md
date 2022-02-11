# Tools for handling hash functions and MACs

Quite a few Challenges deal with hash functions
and message authentication codes based on them.
This module contains some machinery for dealing with them.

```haskell
module Hash
  (
    MAC(..), MACGenerator
  , secretPrefixMAC, validateMAC

  , SHA1Digest(..), sha1Hash
  , SHA1MAC, mkSHA1MAC, validateSHA1MAC
  ) where

import Bytes ( HasBytes(..), Bytes )

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
```
