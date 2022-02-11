# Integers modulo a prime

Lots of more mathematical cryptosystems
are built over mathematical operations on
[the integers modulo a prime](https://en.wikipedia.org/wiki/Modular_arithmetic).

```haskell
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Modulo
  (
    Mod, mkMod, getVal, getModulus
  , modulo, withMod, withMod2
  , recipMod, (^%)
  , MultMod(..), mkMultMod
  ) where

import Util ( xgcd )

import Control.Exception ( throw, ArithException(DivideByZero) )
import Data.Proxy ( Proxy(..) )
import Data.Semigroup ( Semigroup(..) )

import GHC.Real ( numerator, denominator )
import GHC.Natural ( Natural, powModNatural )
import GHC.TypeNats hiding ( Mod(..) )
```

There's lots of ways to implement modular arithmetic.
The implementation I'm going to use is type-safe;
it uses *type-level naturals* to hold the modulus as part of the type,
so the type of integers modulo `p` is `Mod p`.
(It turns out to be similar to the implementation in the
[mod](https://hackage.haskell.org/package/mod) package,
without any of that package's optimizations.)

(Note that because of
[a GHC bug](https://gitlab.haskell.org/ghc/ghc/issues/16586),
this modular-number code doesn't work reliably in GHC versions below 8.8.)

Our modular number type contains a single `Natural` number.
We use a `Natural` to enforce that the value is non-negative;
we interact with it, however, with `Integer`s.

```haskell
newtype Mod (p :: Nat) = Mod { runMod :: Natural }
  deriving (Eq,Ord)
```

We read the value as an `Integer` using `getVal`.

```haskell
getVal :: Mod p -> Integer
getVal (Mod n) = fromIntegral n
```

We read the modulus as an `Integer` using `getModulus`.
We pull the value from the type using `natVal`.

```haskell
getModulus :: KnownNat p => Mod p -> Integer
getModulus m = fromIntegral (natVal m)
```

The `mkMod` function injects an integral value into a `Mod p`.

```haskell
mkMod :: (KnownNat p, Integral a) => a -> Mod p
```

We can actually give `getModulus`
the return value of `mkMod` itself as a proxy.

```haskell
mkMod x = m where m = Mod (fromInteger $ fromIntegral x `mod` getModulus m)
```

This machinery works fine for an explicitly stated modulus:

```haskell ignore
getVal (mkMod 12 :: Mod 7) == 5
```

Usually, however, we want to either generate the prime modulus
or at least be able to choose one at runtime.
We can get the integer value given any prime using the function `modulo`,
which associates the `Mod` expression with a particular modulus.
This uses `someNatVal` to create a proxy with the given value as a type.

```haskell
modulo :: (forall p. KnownNat p => Mod p) -> Integer -> Integer
m `modulo` p = case someNatVal (fromInteger p) of
  SomeNat (_ :: Proxy p) -> getVal (m :: Mod p)
```

`modulo` is a special case of a more general function `withMod`,
which runs a given function f with a modular argument.

```haskell
withMod :: forall h b. Integer
        -> (forall p1. KnownNat p1 => h p1 -> b)
        -> (forall p2. KnownNat p2 => h p2) -> b
withMod p f mx = case someNatVal (fromInteger p) of
  SomeNat (_ :: Proxy p) -> f (mx :: h p)
```

`withMod2` does the same, but with the phantom type parameter
in an internal position in a higher-order type.

```haskell
withMod2 :: forall h f b. Integer
         -> (forall p1. KnownNat p1 => f (h p1) -> b)
         -> (forall p2. KnownNat p2 => f (h p2)) -> b
withMod2 p f mx = case someNatVal (fromInteger p) of
  SomeNat (_ :: Proxy p) -> f (mx :: f (h p))
```

## Typeclasses for Mod

Our `Show` instance displays both the value and the modulus.

```haskell
instance KnownNat p => Show (Mod p) where
  show m@(Mod n) = show n ++ " (mod " ++ show (natVal m) ++ ")"
```

The instance of the `Num` class for `Mod` is straightforward.
Most operations are performed on `Integer`s,
then turned into `Mod`s with  `mkMod`.

```haskell
instance KnownNat p => Num (Mod p) where
  fromInteger = mkMod
  m@(Mod a) + (Mod b) = mkMod (a + b)
  m@(Mod a) * (Mod b) = mkMod (a * b)
```

For `negate` we just perform the subtraction of the value
from the modulus.

```haskell
  negate m@(Mod a) = mkMod (natVal m - a)
```

We don't implement `abs` or `signum`,
which don't really make sense in modular arithmetic.

```haskell
  abs = error "abs not implemented for Mod"
  signum = error "signum not implemented for Mod"
```

Implementing the 'Fractional' class involves an implementation of
modular division, which is a little trickier.
Not every value has an inverse with respect to every modulus.
`x (mod p)` only has an inverse if the greatest common denominator
of `x` and `p` is 1.
The helper function `recipMod` finds the inverse, if there is one.

```haskell
recipMod :: KnownNat p => Mod p -> Maybe (Mod p)
recipMod m =
```

We can use the
[extended Euclidean algorithm](https://en.wikipedia.org/wiki/Euclidean_algorithm#Extended_Euclidean_algorithm)
to find numbers `k` and `l` such that `k*x + l*p = g`,
where `g` is the greatest common divisor of `x` and `p`.

```haskell
  let (g,(k,_)) = xgcd (getVal m) (getModulus m)
```

If `x` and `p` are relatively prime, i.e. if `g == 1`,
then we see that `k*x = 1 (mod p)`, so `k` is the inverse of `x`.

```haskell
  in  case g of
        1 -> Just (mkMod k)
```

Otherwise, there is no inverse.

```haskell
        _ -> Nothing
```

Using `recipMod`, the `Fractional` class is straightforward.

```haskell
instance KnownNat p => Fractional (Mod p) where
  recip m = case recipMod m of
    Nothing -> throw DivideByZero
    Just im -> im

  fromRational rat = case denominator rat of
    1 -> mkMod (numerator rat)
    d -> mkMod (numerator rat) / mkMod d
```

## Faster exponentiation

The standard modular exponentiation is actually handled
in Haskell's default `^` operator.
However, GHC provides a faster exponentiation, `powModNatural`,
which can also be used to create a new operator `^%`.

```haskell
(^%) :: (Integral b, KnownNat p) => Mod p -> b -> Mod p
(^%) m@(Mod n) e = case compare e 0 of
```

If the exponent is positive, we just call `powModNatural`.

```haskell
  GT -> Mod $ powModNatural n (fromIntegral e) (natVal m)
```

If the exponent is negative, we (try to) invert the base
and take the power of the inverse.

```haskell
  LT -> case recipMod m of
         Nothing -> throw DivideByZero
         Just (Mod i) -> Mod $ powModNatural i (fromIntegral $ -e) (natVal m)
```

Otherwise, we return 1.

```haskell
  EQ -> 1
```

We give our new operator the same fixity as the default `^`:

```haskell
infixr 8 ^%
```

## Multiplicative group mod p

The `MultMod` newtype implements `Monoid` with modular multiplication.

```haskell
newtype MultMod (p :: Nat) = MultMod { getMultMod :: Mod p }
  deriving (Eq,Ord,Show)

instance KnownNat p => Semigroup (MultMod p) where
  (MultMod a) <> (MultMod b) = MultMod (a * b)
  stimes k (MultMod a) = MultMod (a ^% k)

instance KnownNat p => Monoid (MultMod p) where
  mempty = MultMod 1
```

We add a function `mkMultMod` to more easily inject an integer:

```haskell
mkMultMod :: (KnownNat p, Integral a) => a -> MultMod p
mkMultMod = MultMod . mkMod
```
