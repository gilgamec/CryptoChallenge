# Generating hash collisions

The hash Challenges in Set 7 require us to find hash collisions:
different blocks which hash to the same thing.
This module contains general functions for doing this.

```haskell
module Hash.Collision
  (
    findCollision
  , multiBlockCollision
  ) where

import Data.List ( mapAccumL )

import qualified Data.Map as M
```

We generate hash collisions by, generally speaking,
evaluating a function on many arguments,
recording the results, and looking for repeated results.
This is fairly easily done with a dictionary from results to arguments;
we'll use a `Map`.
This can be a fairly general function that takes only a function
and a list of arguments, and returns a collision.

```haskell
findCollision :: Ord b => (a -> b) -> [a] -> Maybe ((a,a), b)
findCollision f = go M.empty
 where
```

We go through the list of arguments one by one,
applying the hash function and checking the map for collisions.

```haskell
  go _ [] = Nothing
  go m (x:xs) =
    let h = f x
    in  case m M.!? h of
```

If we find no collisions, we insert a relation between the hash value
and the argument and continue to the next argument.

```haskell
      Nothing -> go (M.insert h x m) xs
```

Otherwise, we return the collision: the pair of colliding arguments
and the collided hash value.

```haskell
      Just x' -> Just ((x, x'), h)
```

We can easily chain together collisions to create a sequence of
colliding values, given a sequence of candidate arguments.

```haskell
multiBlockCollision :: Ord b => (b -> a -> b) -> b -> [[a]] -> [(a, a)]
multiBlockCollision f iv = snd . mapAccumL nextBlock iv
 where
```

From the current hash value and the list of possible blocks,
we generate a collision and return it, along with the collided hash value.

```haskell
  nextBlock iv xs =
    let Just ((x1,x2),h) = findCollision (f iv) xs
    in  (h, (x1,x2))
```
