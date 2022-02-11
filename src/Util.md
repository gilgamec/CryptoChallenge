# Simple utility functions

This module contains some simple utility functions
used by several of the Challenges.

```haskell
module Util
  (
    argmax
  ) where

import Data.Semigroup ( Arg(..), Max(..) )
```

`argmax` applies a function to a bunch of keys
and returns the key that produced the maximum value of the function.

```haskell
argmax :: (Ord v, Foldable f) => (k -> v) -> f k -> k
argmax f xs = case foldMap (\k -> Just $ Max $ Arg (f k) k) xs of
  Nothing -> error "argmax: empty structure"
  Just (Max (Arg _ k)) -> k
```
