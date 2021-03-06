# Padding for hash functions

This module contains functions to add padding for cryptographic hashes.

```haskell
module Padding.Hash
  (
    sha1Padding, padSHA1
  , md4Padding, padMD4
  ) where

import Bytes ( HasBytes(..), Bytes )
import Bytes.Integral ( bigEndian, littleEndian )

import qualified Data.ByteString as B
```

According to [RFC3174](https://tools.ietf.org/html/rfc3174),
SHA-1 padding consists of:

- A single '1' bit;
- Enough '0' bits to fill out the final block to 448 bits (56 bytes)
- The last 64 bits (8 bytes) are taken by the length (in bits)
  of the original mesage and are presented in 64-bit big-endian
  ("If l < 2^32 then the first word is all zeroes.")

In practice then, we append to a w-byte message:

```haskell
sha1Padding :: Int -> Bytes
sha1Padding w =
```

a single byte of value 128 (a single '1' followed by seven '0's);

```haskell
  B.singleton 128 <>
```

(-(w + 9) mod 64) bytes of zero;


```haskell
  B.replicate ((-(w + 9)) `mod` 64) 0 <>
```

and finally the big-endian 64-bit integer representation of 8w.

```haskell
  bigEndian 8 (w * 8)
```

```haskell
padSHA1 :: HasBytes text => text -> Bytes
padSHA1 text =
  let bs = toBytes text
  in  bs <> sha1Padding (numBytes bs)
```

---

[MD4 padding](https://tools.ietf.org/html/rfc1186)
is _almost_ the same as SHA-1.
However, the appended size is in two 32-bit big-endian words,
presented in little-endian order.
("These bits are appended as two 32-bit words and appended low-order
  word first in accordance with the previous conventions.")

*The cryptonite implementation, however, seems to use pure little endian.*

```haskell
md4Padding :: Int -> Bytes
md4Padding w = B.singleton 128 <>
               B.replicate ((-(w + 9)) `mod` 64) 0 <>
               littleEndian 8 (w * 8)

padMD4 :: HasBytes text => text -> Bytes
padMD4 text =
  let bs = toBytes text
  in  bs <> md4Padding (numBytes bs)
```
