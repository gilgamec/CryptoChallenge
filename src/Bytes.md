# Bytes and the HasBytes class

The primary type of data we're working with in the Crypto Challenge
is "a sequence of bytes".
This module defines the `Bytes` type, which holds a byte sequence,
and the `HasBytes` class,
which lets us convert between different representations.

```haskell
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Bytes
  (
    Byte, Bytes, HasBytes(..), convBytes
  , xor, xorb
  , chunksOf
  , takeEnd, dropEnd, splitEnd
  ) where

import Data.Bits ( xor )
import Data.List ( unfoldr )
import Data.Word ( Word8 )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
```

We use `ByteString` to represent a sequence of bytes.
A single byte is then a `Word8`.

```haskell
type Byte = Word8
type Bytes = B.ByteString
```

Bytes can show up and be used, however, in many representations:
as raw ASCII encoded text, in hexadecimal or Base64 encoding,
as integers (big- or little-endian), as polynomials over Galois fields.
To comfortably perform these conversions,
we make all of these representations instances of a typeclass
which lets us convert to and from `Bytes`.

```haskell
class HasBytes b where
  toBytes :: b -> Bytes
  fromBytes :: Bytes -> b
```

The typeclass also includes `numBytes`,
which counts the number of bytes in the `Bytes` representation,
with the obvious default implementation.

```haskell
  numBytes :: b -> Int
  numBytes = B.length . toBytes
```

Raw bytes themselves are trivial instances of this class.

```haskell
instance HasBytes Bytes where
  toBytes = id
  fromBytes = id
```

The default conversion from `String`s to `ByteString`s
 (used in `ByteString`'s `IsString` instance)
packs only the lower byte of each `Char` into the `ByteString`.
The same conversion will be used for the `HasBytes` instance of `String`.

```haskell
instance HasBytes String where
  toBytes = BC.pack
  fromBytes = BC.unpack
```

The helper function `convBytes` converts from one representation to another
by converting to `Bytes` and back.

```haskell
convBytes :: (HasBytes a, HasBytes b) => a -> b
convBytes = fromBytes . toBytes
```

---

The most-used operation on `Bytes` is `xorb`,
which XORs two sequences of `Byte`s byte-by-byte.
It's meant to be used on sequences of the same length;
if the lengths differ,
the answer will be the same length as the shorter sequence.

(The implementation may look like it's zipping into a list
then packing back into a `ByteString`,
but the two operations are in fact fused in
[bytestring](https://hackage.haskell.org/package/bytestring).)

```haskell
xorb :: (HasBytes a, HasBytes b) => a -> b -> Bytes
xorb a b = B.pack $ B.zipWith xor (toBytes a) (toBytes b)
```

---

`chunksOf` splits a byte sequence into blocks of the given size
(though the final block might be smaller if the total length is not a
multiple of the block size).
Since many encryption schemes are block-based,
this will see frequent use.

```haskell
chunksOf :: HasBytes a => Int -> a -> [Bytes]
chunksOf k = unfoldr nextBytes . toBytes
 where
  nextBytes bs
    | B.null bs = Nothing
    | otherwise = Just $ B.splitAt k bs
```

---

We use the `ByteString` equivalents of functions like
`take` and `drop` frequently;
however, we also often need to take a certain number of
`Byte`s from the *end* of the sequence.
We can do this by computing the total length and subtracting,
but it's nice to have helper functions to handle things like this.

```haskell
takeEnd :: Int -> Bytes -> Bytes
takeEnd n bs = B.drop (numBytes bs - n) bs

dropEnd :: Int -> Bytes -> Bytes
dropEnd n bs = B.take (numBytes bs - n) bs

splitEnd :: Int -> Bytes -> (Bytes,Bytes)
splitEnd n bs = B.splitAt (numBytes bs - n) bs
```
