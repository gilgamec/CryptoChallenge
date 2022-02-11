# Bytes representation of integral values

This module defines the `HasBytes` instances for integral values,
as well as little-endian and big-endian conversions.

```haskell
module Bytes.Integral
  (
    integralFromBytes, finiteToBytes
  , bigEndian, littleEndian
  ) where

import Bytes ( HasBytes(..), Bytes, Byte, takeEnd )
import Util ( cdiv )

import Data.Bits ( Bits(..), FiniteBits(..) )
import Data.List ( foldl', unfoldr )
import Data.Word ( Word32 )

import Math.NumberTheory.Logarithms ( integerLog2 )

import qualified Data.ByteString as B
```

The `HasBytes` instances for integral types read and create
**big-endian** byte strings.

All of them use two byte-by-byte bitwise conversions:
converting to `Bytes` uses `unfoldr` of the function `nextIntToByte`:

```haskell
nextIntToByte :: (Bits a, Integral a) => a -> Maybe (Byte, a)
nextIntToByte 0 = Nothing
nextIntToByte k = Just (fromIntegral k, k `shiftR` 8)
```

Converting from `Bytes` uses a `foldl` of the function `nextByteToInt`:

```haskell
nextByteToInt :: (Bits a, Integral a) => a -> Byte -> a
nextByteToInt acc b = (acc `shiftL` 8) .|. fromIntegral b
```

The conversion from `Bytes` works for all `Integral`s:

```haskell
integralFromBytes :: (Bits a, Integral a) => Bytes -> a
integralFromBytes = B.foldl' nextByteToInt 0
```

For finite-width `Integral` types we can also use common functions
in the `HasBytes` instance:

```haskell
finiteToBytes :: (FiniteBits a, Integral a) => a -> Bytes
finiteToBytes n = B.reverse $ fst $ B.unfoldrN totalBytes nextIntToByte n
 where
  totalBytes = finiteNumBytes n

finiteNumBytes :: (FiniteBits a, Integral a) => a -> Int
finiteNumBytes n = finiteBitSize n `cdiv` 8

instance HasBytes Int where
  toBytes = finiteToBytes
  fromBytes = integralFromBytes
  numBytes = finiteNumBytes

instance HasBytes Word32 where
  toBytes = finiteToBytes
  fromBytes = integralFromBytes
  numBytes = finiteNumBytes
```

For `Integer`, we say arbitrarily that
we can only convert positive values.
(Everything we have to convert for these Challenges is positive.)
These conversions use only as many bytes as necessary
to represent the number.

```haskell
instance HasBytes Integer where
  toBytes n = case compare n 0 of
    LT -> error "Cannot convert negative Integer to Bytes"
    EQ -> B.singleton 0
    GT -> B.reverse (B.unfoldr nextIntToByte n)
  fromBytes = integralFromBytes
```

To compute the number of bytes taken up by an `Integer`,
we use its logarithm base 2, computed by the
[integer-logarithms](https://hackage.haskell.org/package/integer-logarithms)
package.

```haskell
  numBytes n = case compare n 0 of
    LT -> error "Cannot convert negative Integer to Bytes"
    EQ -> 1
    GT -> (integerLog2 n + 1) `cdiv` 8
```

---

The integral instances of `HasBytes` create big-endian encodings
of only as many `Byte`s as needed to encode the value.
Sometimes we want little-endian encodings, or encodings of fixed bit sizes.
For this purpose we use the `bigEndian` and `littleEndian` functions,
whose first argument is the number of bytes required.

`bigEndian` trims or pads higher-order bits as necessary.

```haskell
bigEndian :: HasBytes a => Int -> a -> Bytes
bigEndian wid val =
  let bs = toBytes val
      dw = wid - B.length bs
  in  if dw < 0
      then takeEnd wid bs
      else B.replicate dw 0 <> bs
```

`littleEndian` reverses the `Byte`s returned by `bigEndian`.

```haskell
littleEndian :: HasBytes a => Int -> a -> Bytes
littleEndian wid val = B.reverse (bigEndian wid val)
```
