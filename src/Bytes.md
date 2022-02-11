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
  ) where

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
