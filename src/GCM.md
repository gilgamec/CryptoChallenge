# Galois/Counter Mode encryption

This module contains an implementation of
[Galois/Counter Mode](https://en.wikipedia.org/wiki/Galois/Counter_Mode)
encryption, which is based on polynomial evaluation in the Galois field
of size 2^128.

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GCM
  (
    GF(..)
  , gcmHash, GCMMAC, mkGCMMACHash
  , gcmCTR, decryptGCMCTR
  ) where

import Bytes ( HasBytes(..), Bytes, convBytes, chunksOf )
import Bytes.Integral ( integralFromBytes, finiteToBytes, bigEndian )
import Hash ( MAC(..) )
import AES ( encryptECB, encryptCTR, decryptCTR )
import Util ( cdiv )

import Control.Exception ( throw, ArithException(DivideByZero) )
import Data.Bits ( Bits(..), FiniteBits(..) )
import Data.List ( foldl' )
import GHC.Real ( numerator , denominator )

import Data.WideWord.Word128 ( Word128(..) )

import qualified Data.ByteString as B
```

## Math in GF(2^128)

`GF` is an element of the Galois field of order 2^128.
This corresponds to a 127-degree polynomial over Z2,
and is represented by a 128-bit string.
We use `Word128` from the
[wide-word](https://hackage.haskell.org/package/wide-word) package.

```haskell
newtype GF = GF { unGF :: Word128 }
  deriving (Eq, Ord, Show, Bits, FiniteBits)
```

`Word128` is an instance of `FiniteBits`,
so we can use a simple `HasBytes` instance.
We use `bigEndian` to ensure that we always get a 16-byte block from a `GF`.

```haskell
instance HasBytes GF where
  toBytes = bigEndian 16 . finiteToBytes . unGF
  fromBytes = GF . integralFromBytes
  numBytes _ = 16
```

We store the GF lowest-byte first, so the degree of a GF is
the bit position (from the right) of the last 1.

```haskell
gfDegree :: GF -> Int
gfDegree p = 127 - countTrailingZeros p
```

Mathematical operations with GF are polynomial operations
modulo the 128-degree polynomial

    x^128 + x^7 + x^2 + x + 1.

`gfModulus` is the part of the modulus that fits in a GF,
and is used in implementing the `Num` instance.

```haskell
gfModulus :: GF
gfModulus = GF (225 `shiftL` 120)
```

Except for multiplication, it's pretty straightforward.

```haskell
instance Num GF where
```

In converting from an integer,
we use only the constant part of the polynomial;
we thus only care if the input integer is even or odd.

```haskell
  fromInteger n
    | even n    = GF (fromInteger 0)
    | otherwise = GF (1 `shiftL` 127)
```

Negate, absolute value, and signum in Z2 are all id.

```haskell
  negate = id ; abs = id ; signum = id
```

Addition and subtraction are just XOR.

```haskell
  (+) = xor ; (-) = xor
```

The multiplication is mod `gfModulus`;
we implement it after the algorithm given in the Challenge text.

```haskell
  a * b = go 127 0 b
   where
    go (-1) p _ = p
    go i    p q =
      let p' = if testBit a i then p+q else p
          q1 = shiftR q 1
          q' = if testBit q 0 then q1 + gfModulus else q1
      in  go (i-1) p' q'
```

Division returns both quotient and remainder;
its implementation is again from the Challenge text.

```haskell
gfQuotRem :: GF -> GF -> (GF,GF)
gfQuotRem a b =
  let da = gfDegree a
      db = gfDegree b
      nextBit (q,r) d
        | testBit r (127-d) = ( q + (1 `shiftR` (d - db))
                              , r + (b `shiftR` (d - db)) )
        | otherwise         = (q,r)
  in  foldl' nextBit (0,a) [da,da-1..db]
```

We can use this function to implement `Fractional` for `GF`s.

```haskell
instance Fractional GF where
```

`fromRational` just uses `fromInteger` and division.

```haskell
  fromRational rat = (fromInteger $ numerator rat) /
                     (fromInteger $ denominator rat)
```

The reciprocal of p is found using the extended GCD with the modulus,
just like in modular arithmetic.

```haskell
  recip p = case pgcd (p,(0,1)) (r0+r1, (1,q0+q1)) of
              (1, (_,z)) -> z
              _ -> throw DivideByZero
   where
```

Since we can't express the actual modulus as an element of GF,
we must instead start the XGCD algorithm at the second step.
We thus implement a version of that algorithm that lets us start
with any values for s and t.

```haskell
    pgcd rst (0,_) = rst
    pgcd (r1, (s1,t1)) (r2, (s2,t2)) =
      let (q,r) = r1 `gfQuotRem` r2
      in  pgcd (r2, (s2,t2)) (r, (s1 - q*s2, t1 - q*t2))
```

The first step of the algorithm would normally work on

    (gfModulus + x^128, (1,0))  (p, (0,1))

so after this step, we would have

	(p, (0,1))   (r, (1, -q))

where `(q,r) = quotRem (gfModulus + x^128) p`.
But division is linear in the numerator,
so we can just compute `quotRem gfModulus p` and `quotRem x^128 p`
separately and add the results. The former is just

```haskell
    (q0,r0) = gfModulus `gfQuotRem` p
```

while the latter is

    (x^(128 - degree p), (p - x^(degree p)) * x^(128 - degree p))

```haskell
    degDiff = 128 - gfDegree p
    (q1,r1) = (1 `shiftR` degDiff , p `shiftR` degDiff)
```

## GCM hashing

The GCM hash function is just a polynomial in h.
We can implement it using a simple fold over the message chunks
expressed as members of GF(2^128).

```haskell
gcmHash :: HasBytes text => GF -> text -> GF
gcmHash h = foldl' (\g b -> (g + b) * h) 0 . map convBytes . chunksOf 16
```

GCM-MAC has both an encrypted message and associated (plaintext) data,
with a 96-bit nonce.

```haskell
type GCMMAC nonce ad = MAC (nonce, ad, Bytes) GF
```

The hash for a GCMMAC is generated by the function `mkGCMMACHash`.

```haskell
mkGCMMACHash :: (HasBytes key, HasBytes nonce, HasBytes ad)
             => key -> (nonce,ad,Bytes) -> GF
mkGCMMACHash key (nonce,ad,ct) = hash
 where
  blockSize = 16
```

We need two elements of GF. First is the authentication key h,
which is the encryption of the zero block:

```haskell
  Just h = fromBytes <$> encryptECB key (B.replicate blockSize 0)
```

Second is the constant term s, which is encrypted from the 96-bit nonce:

```haskell
  Just s = fromBytes <$> encryptECB key (toBytes nonce <> B.pack [0,0,0,1])
```

We need to pad our associated data and ciphertext to whole blocks:

```haskell
  zeroPad bs =
    let sz = numBytes bs
        pad = blockSize * (sz `cdiv` blockSize) - sz
    in  toBytes bs <> B.replicate pad 0
```

We put together the associated data (padded to a whole block),
ciphertext (likewise), with the lengths in bits of each.
This is the message we're going to hash.

```haskell
  hashText = B.concat [ zeroPad ad, zeroPad ct
                      , bigEndian 8 (8 * numBytes ad)
                      , bigEndian 8 (8 * numBytes ct) ]
```

Finally, we call `gcmHash` to hash the text using h as the "key"
(i.e. the value of x) and add s (i.e. as the constant term).

```haskell
  hash = gcmHash h hashText + s
```

GCM-CTR is just a standard CTR, with a GCM MAC authentication.

```haskell
gcmCTR :: (HasBytes key, HasBytes nonce, HasBytes ad, HasBytes text)
       => key -> nonce -> ad -> text -> GCMMAC nonce ad
gcmCTR key nonce ad text =
  let Just ct = encryptCTR key nonce text
      msg = (nonce,ad,ct)
      hash = mkGCMMACHash key msg
  in  MAC{ macMessage = msg, macHash = hash }
```

```haskell
decryptGCMCTR :: (HasBytes key, HasBytes nonce, HasBytes ad)
              => key -> GCMMAC nonce ad -> Maybe (ad,Bytes)
decryptGCMCTR key mac =
  let MAC{ macMessage = (nonce,ad,ct), macHash = hash } = mac
      Just pt = decryptCTR key nonce ct
  in if hash == mkGCMMACHash key (nonce,ad,ct)
     then Just (ad,pt)
     else Nothing
```
