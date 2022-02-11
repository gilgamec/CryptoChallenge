# Merkle-Damgard hashes of tunable strength

Some of the Challenges deal with collisions in Merkle-Damgard hashes.
In order to deal with these in a toy fashion,
this module implements simple Merkle-Damgard hashes
where the strength (size of the hash state) is tunable.

```haskell
module Hash.MerkleDamgard
  (
    mdHashOne
  , mdHash
  , mdIV
  ) where

import Bytes ( HasBytes(..), Bytes, chunksOf )
import AES ( encryptECB )
import Padding.Hash ( padSHA1 )

import Data.List ( foldl' )
import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
```

For each message block M, the hash state H is modified by H <- C(M,H).
We'll use AES as the base function and modify the strength
by cutting the hash state to fewer than 16 bytes.

```haskell
mdHashOne :: Int -> Bytes -> Bytes -> Bytes
mdHashOne hashSize h m =
```

We have to pad the hash state up to 16 blocks on the way in
and trim it back on the way out.

```haskell
  let paddingLen = 16 - hashSize
      pad b = B.replicate paddingLen 0 <> b
      trim b = B.drop paddingLen b
```

We then use the padded state as a key
to encrypt the message block into our next state.

```haskell
  in  trim $ fromJust $ encryptECB (pad h) m
```

The hash function iterates `mdHashOne`,
starting with a fixed initial state.
To turn the input into full blocks, we use SHA-1 padding.

```haskell
mdHash :: HasBytes text => Int -> text -> Bytes
mdHash hashSize text =
  let blocks = chunksOf 16 $ padSHA1 text
  in  foldl' (mdHashOne hashSize) (mdIV hashSize) blocks
```

The initial state is just going to be all zeros.

```haskell
mdIV :: Int -> Bytes
mdIV n = B.replicate n 0
```
