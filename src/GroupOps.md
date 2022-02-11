# Group-based operations

This file contains operations and attacks which
work not only for the multiplicative group modulo p,
but for any group (in fact, any `Monoid`).
This means that we can use them to attack
cryptography over the integers modulo p,
or elliptic curve groups.

```haskell
{-# LANGUAGE BangPatterns #-}

module GroupOps
  (
    lowOrderBase
  , pohligHellmanOracle
  ) where

import Math ( crt, smallFactors )

import Control.Monad ( forM )
import Data.Semigroup ( stimes )

import qualified Control.Monad.Random as R
import qualified Data.Vector as V
```

## Utilities

`lowOrderBase` produces an element of a group which has
a given multiplicative order r,
where r is prime and divides the order of the group.
This requires us to be able to generate random elements of the group.

```haskell
lowOrderBase :: (R.MonadRandom m, Monoid a, Eq a)
             => m a -> Integer -> Integer -> m a
lowOrderBase mkRan o r =
```

If the group order o = r * k,
then for any element h of the group, h^o = ( h^k )^r = 1,
i.e. the order of h^k divides r, which means it's either 1 or r.
If we pick any random h, then take it to the power k,
we just have to ensure that it's not one;
otherwise, its order must be r.

```haskell
  let mkBase k = do
        h <- mkRan
        let g = k `stimes` h
        if g == mempty
          then mkBase k
          else pure g
```

Then we just have to compute k = o / r;
if the division isnt' exact, we fail.

```haskell
  in  case o `quotRem` r of
        (k,0) -> mkBase k
        _     -> error $ "lowOrderBase: target order "++show r++
                         " does not divide group order "++show o
```

## Pohlig-Hellman

The [Pohlig-Hellman algorithm](https://en.wikipedia.org/wiki/Pohlig%E2%80%93Hellman_algorithm)
solves the discrete logarithm problem;
that is, it finds k such that h^k = g in some discrete cyclic group,
like multiplication modulo a prime.

`pohligHellmanOracle` is used when we don't know the value y
whose discrete logarithm is required;
it instead uses an oracle to tell us if we've found a pair
(h,x) such that h^x = y.
It's also specialized to work on groups whose orders
have many small primes (less than 2^16) as factors.
The function needs a way to generate a random element of the group
and an oracle to see if an element to the given power is equal to our target.
It returns a pair (x,r), where r divides the order of the group
and x and the true discrete logarithm are congruent modulo r.

```haskell
pohligHellmanOracle :: (R.MonadRandom m, Monoid n, Eq n)
                    => m n -> (n -> Integer -> Bool) -> Integer
                    -> m (Integer,Integer)
pohligHellmanOracle mkRan oracle o = do
```

We pull out small factors of the order.
We only want single prime factors;
multiple primes significantly complicate matters.

```haskell
  let primeBound = 2^16
      rs = [ fac | (fac,1) <- fst $ smallFactors primeBound o ]
```

Each r should be small enough that we can just brute-force the oracle.

```haskell
  let dlog r h = head $ filter (oracle h) [0..r-1]
```

For each factor, we find a base of that order
and then take the logarithm with respect to it.
Note that this means that b = x mod r.

```haskell
  bs <- forM rs $ \r -> do
    h <- lowOrderBase mkRan o r
    pure (dlog r h)
```

Finally, we take the CRT of these logarithms;
this gives us a number congruent to x mod (product of rs).

```haskell
  let m = product rs
  pure (crt (zip bs rs) `rem` m, m)
```
