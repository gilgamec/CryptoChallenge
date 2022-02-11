# Simple utility functions

This module contains some simple utility functions
used by several of the Challenges.

```haskell
module Util
  (
    argmax, argmin, argmaxA
  , allPairs, seqPairs
  , countRepeats
  , cdiv, xgcd
  ) where

import Data.List ( tails )
import Data.Monoid ( Ap(..) )
import Data.Ord ( Down(..) )
import Data.Semigroup ( Arg(..), Max(..) )
```

## Argmin / argmax

`argmax` applies a function to a bunch of keys
and returns the key that produced the maximum value of the function.

```haskell
argmax :: (Ord v, Foldable f) => (k -> v) -> f k -> k
argmax f xs = case foldMap (\k -> Just $ Max $ Arg (f k) k) xs of
  Nothing -> error "argmax: empty structure"
  Just (Max (Arg _ k)) -> k
```

`argmin` does the same, but returns the key corresponding to the
minimum function value.

```haskell
argmin :: (Ord v, Foldable f) => (k -> v) -> f k -> k
argmin f = argmax (Down . f)
```

`argmaxA` performs the argmax with a function which operates
in an applicative context.

```haskell
argmaxA :: (Ord v, Traversable f, Applicative m) => (k -> m v) -> f k -> m k
argmaxA f xs = getAp $ getValue <$> foldMap am xs
 where
  am k = Ap $ (Just . Max . (\v -> Arg v k)) <$> f k
  getValue Nothing = error "argmaxA: empty structure"
  getValue (Just (Max (Arg _ k))) = k
```

## Extracting pairs from a list

`allPairs` returns all (in-order) pairs from the list.

    allPairs [1,2,3,4] = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

```haskell
allPairs :: [a] -> [(a,a)]
allPairs as = [ (a,b) | a:bs <- tails as, b <- bs ]
```

`seqPairs` returns all pairs of sequential values from the list.

    seqPairs [1,2,3,4] = [(1,2),(2,3),(3,4)]

```haskell
seqPairs :: [a] -> [(a,a)]
seqPairs xs = zip xs (tail xs)
```

## Other list functions

`countRepeats` counts how many elements in the list are repeats
of other elements.

```haskell
countRepeats :: Eq a => [a] -> Int
countRepeats xs = go xs []
 where
  go [] _ = 0
  go (y:ys) xs
    | y `elem` xs = 1 + go ys xs
    | otherwise   = go ys (y:xs)
```

## Mathematical functions

`cdiv` returns the *ceiling* of the quotient of two Integrals.
This is extremely useful; it gives us (for example) the number of
full or partial blocks needed to hold a given message.

```haskell
cdiv :: Integral a => a -> a -> a
n `cdiv` d =
  let (q,r) = n `quotRem` d
  in  q + signum r
```

`xgcd` finds the *extended GCD* of two numbers;
that is, if `g` is the greatest common divisor of `a` and `b`,
then `xgcd` finds `g` as well as two numbers `p` and `q`
such that `pa + qb = g`.

```haskell
xgcd :: Integral a => a -> a -> (a,(a,a))
xgcd a b = go (a,(1,0)) (b,(0,1))
 where
  go rst (0,_) = rst
  go (r1,(s1,t1)) (r2,(s2,t2)) =
    let q = r1 `div` r2
    in  go (r2,(s2,t2)) (r1 - q*r2, (s1 - q*s2, t1 - q*t2))
```
