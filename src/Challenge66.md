# Solution to Challenge 66

```haskell
{-# LANGUAGE MultiWayIf #-}

module Challenge66
  (
    BWECParameters(..), wecParamsFor
  , bwecOracle
  , findBWECPrivateKey
  ) where

import EllipticCurve ( WECParameters(..), WECPoint(..), mkWEC, modWEC, wecAdd )
import PublicKey ( KeyPair(..), PublicKey(..) )
import PublicKey.ECDiffieHellman ( WECDHParams(..)
                                 , WECDHKeyPair, WECDHPublicKey )
import Random ( randomWECPoint )

import Control.Monad ( foldM )
import Data.Bits ( testBit )
import Data.Maybe ( isJust, listToMaybe )
import Data.Semigroup ( stimes )

import Math.NumberTheory.Logarithms ( integerLog2 )

import qualified Control.Monad.Random as R
```

## Broken elliptic curves

Our "broken WEC" is the same as the normal
Weierstrass elliptic curve implementation,
with one additional parameter,
related to the chance that a multiplication will fail.

```haskell
data BWECParameters = BWECParameters
  { bwecA, bwecB, bwecP :: Integer
  , bwecFail :: Integer }
  deriving (Eq,Ord,Show)
```

We can convert it back to standard `WECParameters`.

```haskell
wecParamsFor :: BWECParameters -> WECParameters
wecParamsFor bwec = WECParameters{ wecA = bwecA bwec
                                 , wecB = bwecB bwec
                                 , wecP = bwecP bwec }
```

Elliptic curve addition just recapitulates the normal WEC addition,
though it might also fail.

```haskell
bwecAdd :: BWECParameters -> WECPoint -> WECPoint -> Maybe WECPoint
```

Here is the failure check:
If the product of the X coordinates is zero modulo the fail parameter,
the addition fails and we return Nothing.

```haskell
bwecAdd params p1@WECPoint{wecX=x1} p2@WECPoint{wecX=x2}
  | (x1 * x2) `rem` bwecFail params == 0 = Nothing
```

Otherwise, we fall back to the ordinary elliptic curve addition.

```haskell
  | otherwise = Just $ wecAdd (wecParamsFor params) p1 p2
```

Multiplication is high-to-low bit, with a chance of failure
encapsulated by folding in the Maybe monad.

```haskell
bwecMult :: BWECParameters -> Integer -> WECPoint -> Maybe WECPoint
bwecMult params k q = foldM nextBit q bits
 where
```

We extract the bits from the multiplier:

```haskell
  n = integerLog2 k
  bits = [ testBit k i | i <- [n-1,n-2..0] ]
```

For each bit, we double the accumulator, then add q if the bit is on.
At any point, this addition may fail the entire computation.

```haskell
  add = bwecAdd params
  nextBit r True  = add r r >>= add q
  nextBit r False = add r r
```

The oracle tests if a given BWEC key generation succeeds or fails
due to the math error.

```haskell
bwecOracle :: BWECParameters -> WECDHKeyPair -> WECPoint -> Bool
bwecOracle params KeyPair{ kpPrivate = d } = isJust . bwecMult params d
```

## Determining the key

The function `diagnosticQ` determines whether
a given point q is diagnostic for an addition failure,
given that we know the first few bits of the private key;
it returns a pair of booleans reporting whether the math will succeed
if the next bit is either of 0 or 1.
Note that this checks the *next* multiplication
(i.e. instead of 2k vs 2k+1, it checks 4k vs 4k+2);
it is thus *not* diagnostic for find the last bit.

```haskell
diagnosticQ :: BWECParameters -> Integer -> WECPoint -> (Bool,Bool)
diagnosticQ params k q =
  let succeeds kVal = isJust (bwecMult params kVal q)
  in  (succeeds (k*4), succeeds (k*4 + 2))
```

If a given point is diagnostic - that is, if its success or failure
depends on the next bit of the key - then we have two possibilities.
If our entire evaluation succeeds, then we know that the next bit
is the one which the diagnostic point succeeds on.
If the evaluation fails, however, then we only know that it *might*
be the one which the point fails on;
it is also possible that the evaluation fails on a later bit.

(Originally, I collected evidence from failed operations as well
as successful ones; with enough failed operations, we could
statistically determine the correct value. However, since half of all
diagnostic operations are successful, this was essentially useless; the code
never had the opportunity to collect enough evidence before it received
definitive proof of the next bit.)

Our knowledge after an evaluation is either `Nothing`,
or `Just` the bit value.

```haskell
type BitEvidence = Maybe Integer
```

'evidence' computes the bit evidence for a given point.
If the point is not diagnostic, then we can't gain any evidence;
otherwise, we run the oracle and see if we get
a success (definitive evidence, one way or the other) or failure.

```haskell
evidence :: BWECParameters -> (WECPoint -> Bool) -> Integer -> WECPoint
         -> BitEvidence
evidence params oracle known q =
```

We first find if the point q is diagnostic for our known part of the key.

```haskell
  let (k0,k1) = diagnosticQ params known q
```

If it is not (i.e. if the oracle would fail or succeed in either case)
then we can get no information.

```haskell
  in  if | k0 == k1  -> Nothing
```

Otherwise, we check if the computation succeeds.
If so, then the bit is a zero exactly when k0 is True, or

```haskell
         | oracle q  -> Just (if k0 then 0 else 1)
```

Finally, if the oracle fails then we don't know what the bit is.

```haskell
         | otherwise -> Nothing
```

`nextDBit` gathers evidence about the next bit
by generating random points and querying the oracle
until we can decide on the next bit. Once the next bit has been determined,
it returns an updated `known` value.

```haskell
nextDBit :: R.MonadRandom m
         => BWECParameters -> (WECPoint -> Bool) -> Integer
         -> m Integer
nextDBit params oracle known = do
  q <- randomWECPoint (wecParamsFor params)
  case evidence params oracle known q of
    Nothing -> nextDBit params oracle known
    Just v  -> pure (known * 2 + v)
```

`findBWECPrivateKey` finds the private key in our broken WEC
by iterating `nextDBit`. We start with the known first bit,
which is certainly a 1.

```haskell
findBWECPrivateKey :: R.MonadRandom m
                   => BWECParameters -> (WECPoint -> Bool) -> WECDHPublicKey
                   -> m Integer
findBWECPrivateKey bParams oracle pk = go 1
 where
  g = wecdhBase (pkParameters pk)
  wParams = wecParamsFor bParams
```

We can check if our known part of the key is complete
by trying to produce the public key after appending both a 0 and a 1.

```haskell
  correctKey d = listToMaybe [ k | b <- [0,1], let k = 2*d + b
                                 , let k' = (k `stimes` mkWEC g) `modWEC` wParams
                                 , k' == pkKey pk ]
```

Using this as a stop criterion, we iterate with the function `go`.

```haskell
  go known = case correctKey known of
    Just d  -> pure d
    Nothing -> nextDBit bParams oracle known >>= go
```
