# Polynomial rings

This module implements basic mathematical operations on polynomials.

```haskell
{-# LANGUAGE DeriveFunctor #-}

module Polynomial
  (
    Poly(..)
  , toPoly, polyX, polyDegree
  , polyDerivative
  , polyEval, polyCoeff
  , polyQuotRem, polyQuot, polyRem
  , polyGCD
  , polyModPow
  ) where

import Data.Function ( on )
import Data.List ( intercalate, foldl' )

import qualified Data.IntMap as IM
```

A polynomial is a map from integers (powers of x) to coefficients.

```haskell
newtype Poly a = Poly { unPoly :: IM.IntMap a }
  deriving (Ord, Functor)
```

`toPoly` turns a scalar into a zero-degree polynomial.

```haskell
toPoly :: a -> Poly a
toPoly = Poly . IM.singleton 0
```

`polyX` is the polynomial 1*x.

```haskell
polyX :: Num a => Poly a
polyX = Poly (IM.singleton 1 $ fromInteger 1)
```

`polyMinify` deletes all map entries with zero coefficients.

```haskell
polyMinify :: (Eq a, Num a) => Poly a -> Poly a
polyMinify (Poly m) = Poly (IM.mapMaybe delZero m)
 where
  delZero 0 = Nothing
  delZero n = Just n
```

`polyDegree` returns the degree of the polynomial, or -1 if it is zero.
It uses the `maxViewWithKey` function to get the largest key.

```haskell
polyDegree :: (Eq a, Num a) => Poly a -> Int
polyDegree = maybe (-1) (fst . fst) . IM.maxViewWithKey . unPoly . polyMinify
```

Equality between polynomials is performed on minified versions.

```haskell
instance (Eq a, Num a) => Eq (Poly a) where
  (==) = (==) `on` (unPoly . polyMinify)
```

The `Show` instance is perhaps a little more than we need,
but certainly aids in debugging.

```haskell
instance (Eq a, Num a, Show a) => Show (Poly a) where
  show 0 = "0"
  show p =
```

We minify the polynomial and pull out the individual terms as pairs
(degree, coefficient).

```haskell
    let terms = IM.toList $ unPoly $ polyMinify p
```

We use the function `showTerm` to show a term.
The constant term is simple:

```haskell
        showTerm (0,a) = show a
```

Higher terms are prettified a little bit based on the value of a.

```haskell
        showTerm (1,1) = "x"
        showTerm (1,-1) = "-x"
        showTerm (1,a) = show a ++ " x"
```

Above linear, we also show the exponent.

```haskell
        showTerm (d,1) = "x^"++show d
        showTerm (d,-1) = "-x^"++show d
        showTerm (d,a) = show a++" x^"++show d
```

We then write our terms (in reverse order, i.e. highest degree to lowest)
with `+` in between.

```haskell
    in  intercalate " + " $ reverse $ map showTerm terms
```

The numeric instance for `Poly` isn't too difficult.

```haskell
instance (Eq a, Num a) => Num (Poly a) where
```

An integer is converted to a constant term,
or to an empty polynomial if it's zero.

```haskell
  fromInteger 0 = Poly IM.empty
  fromInteger n = toPoly (fromInteger n)
```

Negation, absolute value, and signum are all applied term by term.

```haskell
  negate = fmap negate ; abs = fmap abs ; signum = fmap signum
```

Addition is also applied term by term, using `unionWith`.
The resulting polynomial is minified.

```haskell
  (Poly a) + (Poly b) = polyMinify $ Poly (IM.unionWith (+) a b)
```

Finally, multiplication is implemented by folding over each term.

```haskell
  (Poly a) * (Poly b) = polyMinify $ IM.foldlWithKey multOne 0 b
   where
    multOne p n x = p + fmap (x*) (Poly $ IM.mapKeysMonotonic (+n) a)
```

`polyDerivative` returns the derivative of the polynomial with respect to x.

```haskell
polyDerivative :: (Eq a, Num a) => Poly a -> Poly a
polyDerivative (Poly p) = Poly p'
 where
```

The derivative of the term (n,a) is (n-1, n*a).
In order to avoid having to minify, we return `Nothing` for the constant term.

```haskell
  dmult 0 _ = Nothing
  dmult n x = Just (fromIntegral n * x)
```

We change the coefficient with `mapMaybeWithKey`,
and the degree with `mapKeysMonotonic`.

```haskell
  p' = IM.mapKeysMonotonic (subtract 1) $ IM.mapMaybeWithKey dmult p
```

`polyEval` uses Horner's rule to evaluate a polynomial at a value.

```haskell
polyEval :: Num a => Poly a -> a -> a
polyEval (Poly as) x =
  let maxExp = fst $ IM.findMax as
  in  fst $ IM.foldrWithKey' go (0,maxExp) as
 where
  go ix a (acc,oldIx) = (a + x^(oldIx - ix) * acc, ix)
```

`polyCoeff` returns the coefficient of the given degree;
the function just exposes `lookup` for the `IntMap`.

```haskell
polyCoeff :: Poly a -> Int -> Maybe a
polyCoeff (Poly a) = (a IM.!?)
```

`polyQuotRem` returns the quotient and remainder of polynomial division.

```haskell
polyQuotRem :: (Eq a, Fractional a) => Poly a -> Poly a -> (Poly a, Poly a)
polyQuotRem a b = foldl' nextPower (0,a) [da,da-1..db]
 where
  da = polyDegree a
  db = polyDegree b
  Just bn = polyCoeff b db
  nextPower (q,r) d = case polyCoeff r d of
    Nothing -> (q,r)
    Just ad ->
      let mult = Poly $ IM.singleton (d-db) (ad / bn)
      in  (q + mult, r - mult * b)
```

We can split this up to return just the quotient
or just the remainder.

```haskell
polyQuot, polyRem :: (Eq a, Fractional a) => Poly a -> Poly a -> Poly a
polyQuot a b = fst $ polyQuotRem a b
polyRem  a b = snd $ polyQuotRem a b
```

`polyGCD` returns the greatest common divisor of two polynomials.

```haskell
polyGCD :: (Eq a, Fractional a) => Poly a -> Poly a -> Poly a
polyGCD a 0 = a
polyGCD a b = polyGCD b (polyRem a b)
```

`polyModPow` finds the modular exponent of a polynomial
by repeated squaring.

```haskell
polyModPow :: (Eq a, Fractional a) => Poly a -> Integer -> Poly a -> Poly a
polyModPow _ 0 _ = 1
polyModPow a 1 m = a `polyRem` m
polyModPow a k m =
  let (q,r) = k `quotRem` 2
      rmult = if r == 1 then a else 1
      ak2   = polyModPow a q m
  in  (rmult * ak2 * ak2) `polyRem` m
```
