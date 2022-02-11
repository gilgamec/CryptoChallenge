# Elliptic curves

This module implements the mathematics of elliptic curves.

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module EllipticCurve
  (
    WECParameters(..)
  , WECPoint(..), mkWECPoint
  , wecInverse, wecAdd

  , WEC(..), getWEC, mkWEC, invWEC
  , modWEC, withWEC, withWEC2
  ) where

import Modulo ( mkMod, getVal, modulo, withMod, (^%) )

import Data.List ( unfoldr )
import Data.Reflection ( Reifies(..), reify )
import Data.Proxy ( Proxy )
import Data.Semigroup ( Semigroup(..), stimesMonoid )
```

## Weierstrass formulation

A Weierstrass-form elliptic curve is a curve in (x,y) of the form

    y^2 = x^3 + ax + b

If we take x and y to be in Zp, then we have three parameters: p, a, and b.

```haskell
data WECParameters =
  WECParameters{ wecA, wecB, wecP :: Integer }
  deriving (Eq,Ord,Show)
```

Points on the curve are identified by their (x,y) coordinates.
However, there is also a "point at infinity" which acts as the group identity.
We will let an elliptic curve point be either zero (the identity)
or a coordinate (x,y).

```haskell
data WECPoint =
    WECZero
  | WECPoint{ wecX, wecY :: Integer }
  deriving (Eq,Ord,Show)
```

We create a new (finite) point with `mkWECPoint`.

```haskell
mkWECPoint :: WECParameters -> Integer -> Integer -> WECPoint
mkWECPoint params x y = WECPoint (x `mod` wecP params) (y `mod` wecP params)
```

The inverse of a finite point is the point with the same x coordinate
and negative the y coordinate.

```haskell
wecInverse :: WECParameters -> WECPoint -> WECPoint
wecInverse _ WECZero = WECZero
wecInverse _ (WECPoint x 0) = WECPoint x 0
wecInverse params (WECPoint x y) = WECPoint x (wecP params - y)
```

The math behind elliptic curve point addition is not especially tough.
The basic rule is that if we have three points P Q R on the curve,
the lie on the same line iff P + Q + R = 0; that is, P + Q = -R,
the inverse of the third point on the curve.

```haskell
wecAdd :: WECParameters -> WECPoint -> WECPoint -> WECPoint
```

1. Any point plus the zero point is itself.

```haskell
wecAdd _ p1 WECZero = p1
wecAdd _ WECZero p2 = p2
```

2. A point plus its inverse is zero.

```haskell
wecAdd _ (WECPoint x1 0) (WECPoint x2 0)
  | x1 == x2 = WECZero
wecAdd params (WECPoint x1 y1) (WECPoint x2 y2)
  | x1 == x2 && y1 == wecP params - y2 = WECZero
```

3. Otherwise...

```haskell
  | otherwise =
```

All of the arithmetic here is modulo p.

We compute the slope m of the line between the points.
If the points are the same, then we have to use the tangent to the curve;
this is (dC/dx) / (dC/dy) = (3x^2 + a) / (2y).

```haskell
  let m | x1 == x2  = mkMod (3*x1*x1 + wecA params) / mkMod (2*y1)
```

If the points are different the slope is just rise over run.

```haskell
        | otherwise = mkMod (y2 - y1) / mkMod (x2 - x1)
```

Now, x^3 + ax + b = (mx + k)^2 is a cubic equation with three roots:

    x^3 + ax + b - (mx + k)^2 = (x - x1) (x - x2) (x - x3).

The coefficient of x^2 on both sides must be the same, so

    -m^2 = -(x1 + x2 + x3), or
	
```haskell ignore
      x3 = m*m - mkMod x1 - mkMod x2
```

We then substitute into the formula for m and negate to get -R.

```haskell ignore
      y3 = m * (mkMod x1 - x3) - mkMod y1
```

Finally, we apply the modulus to x3 and y3.

```haskell ignore
  in  WECPoint (x3 `modulo` p) (y3 `modulo` p)
```

*For the sake of efficiency, we're going to slightly reorder the above;
otherwise, m is evaluated up to five times per multiplication!*

```haskell
  in  withMod (wecP params)
              (\m -> let x3 = m*m - mkMod x1 - mkMod x2
                         y3 = m * (mkMod x1 - x3) - mkMod y1
                     in  WECPoint (getVal x3) (getVal y3)) m
```

### Working with points

We're going to use the same trick we used for modular arithmetic:
putting the curve parameters into a phantom type,
forcing a given series of `WECPoint` operations to use them.

```haskell
newtype WEC p = WEC { runWEC :: WECPoint }
  deriving (Eq,Ord,Show)

getWEC :: WEC p -> WECPoint
getWEC = runWEC
```

But where is the type parameter going to come from?
GHC has builtin natural number types, but not
builtin elliptic curve parameter types.
However, the [reflection](https://hackage.haskell.org/package/reflection)
package offers a similar facility for *any* type.
It's implemented with weird type-level voodoo and `unsafeCoerce`,
but we don't have to understand any of that to use it.

The constraint `Reifies p WECParameters` states that
type parameter `p` represents a set of `WECParameters`.
We can get the specific parameter set with `reflect`.

```haskell
mkWEC :: Reifies p WECParameters => WECPoint -> WEC p
mkWEC (WECPoint x y) = w where w = WEC (mkWECPoint (reflect w) x y)
```

We make a computation run with a given set of parameters
using `reify`.

```haskell
modWEC :: (forall p. Reifies p WECParameters => WEC p)
       -> WECParameters -> WECPoint
wec `modWEC` p = reify p (\(_ :: Proxy p) -> runWEC (wec :: WEC p))

withWEC :: forall h b. WECParameters
        -> (forall p1. Reifies p1 WECParameters => h p1 -> b)
        -> (forall p2. Reifies p2 WECParameters => h p2) -> b
withWEC p f wec = reify p (\(_ :: Proxy p) -> f (wec :: h p))

withWEC2 :: forall f h b. WECParameters
         -> (forall p1. Reifies p1 WECParameters => f (h p1) -> b)
         -> (forall p2. Reifies p2 WECParameters => f (h p2)) -> b
withWEC2 p f wec = reify p (\(_ :: Proxy p) -> f (wec :: f (h p)))
```

We can now define `Semigroup` and `Monoid` instances for WEC points.

```haskell
instance Reifies p WECParameters => Monoid (WEC p) where
  mempty = WEC WECZero
instance Reifies p WECParameters => Semigroup (WEC p) where
  p1 <> p2 = WEC $ wecAdd (reflect p1) (getWEC p1) (getWEC p2)
```

Since Haskell doesn't provide a default `stimes` for a group with inverse,
we have to catch the `n < 0` case ourselves.

```haskell
  stimes n
    | n < 0     = stimes (negate n) . invWEC
    | otherwise = stimesMonoid n
```

For this, we'll need a group inverse function.

```haskell
invWEC :: forall p. Reifies p WECParameters => WEC p -> WEC p
invWEC w@(WEC p) = WEC $ wecInverse (reflect w) p
```
