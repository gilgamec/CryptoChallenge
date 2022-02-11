# Public-key cryptosystems

A *public-key* cryptosystem has *two* asymmetric keys.
The *public* key is widely known,
while the *private* key is kept secret.
Messages encrypted or signed with one can only be decrypted
or authenticated by the other.

This module contains general machinery for dealing with
public-key cryptosystems.

```haskell
module PublicKey
  (
    KeyPair(..), PublicKey(..)
  , publicPart
  ) where
```

We distinguish three kinds of information in a public-key cryptoscheme.
The first two are the public and private keys;
the third is the *parameters*, which include things like the modulus
for operations.

The `KeyPair` records all three of these.

```haskell
data KeyPair params public private = KeyPair
  { kpParameters :: params
  , kpPublic :: public
  , kpPrivate :: private }
  deriving (Eq,Ord,Show)
```

The `PublicKey` records only the parameters and public key.

```haskell
data PublicKey params public = PublicKey
  { pkParameters :: params
  , pkKey :: public }
  deriving (Eq,Ord,Show)
```

The function `publicPart` extracts the public key from a key pair.

```haskell
publicPart :: KeyPair params public private -> PublicKey params public
publicPart KeyPair{kpParameters = params, kpPublic = pubKey} =
  PublicKey{ pkParameters = params, pkKey = pubKey }
```
