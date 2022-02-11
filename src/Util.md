# Simple utility functions

This module contains some simple utility functions
used by several of the Challenges.

```haskell
module Util
  (
    argmax, argmin
  , allPairs
  ) where

import Data.List ( tails )
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

## Extracting pairs from a list

`allPairs` returns all (in-order) pairs from the list.

    allPairs [1,2,3,4] = [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]

```haskell
allPairs :: [a] -> [(a,a)]
allPairs as = [ (a,b) | a:bs <- tails as, b <- bs ]
```
