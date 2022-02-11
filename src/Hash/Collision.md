# Generating hash collisions

The hash Challenges in Set 7 require us to find hash collisions:
different blocks which hash to the same thing.
This module contains general functions for doing this.

```haskell
module Hash.Collision
  (
    findCollision
  , multiBlockCollision
  , findCollision2
  ) where

import Data.List ( mapAccumL )

import qualified Data.Map as M
```

## Single-function collisions

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

## Two-function collisions

Rather than collisions between different arguments to a single function,
here we're looking for pairwise collisions between two different functions.
The general function `findCollision2` generates these.

```haskell
findCollision2 :: Ord b
               => (a1 -> b) -> [a1]
               -> (a2 -> b) -> [a2]
               -> Maybe ((a1,a2), b)
findCollision2 fx xs fy ys =
```

Because the lists of candidate values are likely to be long
(like, all possible 16-byte blocks),
we don't want to compare `fx(x1)` against `fy` for all `ys`
before moving on to `fx(x2)`.
Instead, we perform the comparisons in a 'square' ordering:
all comparisons of the first n evaluations of the first function
with the first n evaluations of the second
will be completed before evaluating the n+1 element of either function.

We store two `Map`s from function values to arguments, one for each function.
Along with each we carry the list of unevaluated arguments.
We can evaluate the next element into the `Map`,
returning it and its function value:

```haskell
  let evalNext f (m,x:xs) = let h = f x in Just ((x,h), (M.insert h x m, xs))
      evalNext _ (_,[]) = Nothing
```

We evaluate the next element of each list in turn:

```haskell
      nextEvaluations mxs mys = case (evalNext fx mxs, evalNext fy mys) of
```

If neither list has a new value, we have tried every comparison
and must conclude there is no collision.

```haskell
        (Nothing,Nothing) -> Nothing
```

If we got a new value from `xs`, we compare it against the y map
and return if we get a collision.

```haskell
        (Just ((x,hx),_),_)
          | Just y <- M.lookup hx (fst mys) -> Just ((x,y), hx)
```

Similarly, we compare any new y value with the x map.

```haskell
        (_,Just ((y,hy),_))
          | Just x <- M.lookup hy (fst mxs) -> Just ((x,y), hy)
```

If we had new values from both lists,
we have to compare them against each other.

```haskell
        (Just ((x,hx),_), Just ((y,hy),_))
          | hx == hy -> Just ((x,y), hx)
```

Finally, if all of those fall through,
we have to continue to the next evaluations
with either the old maps and lists (if there are no elements left to evaluate)
or the new ones (if they weren't empty yet).

```haskell
        (mbex,mbey) -> nextEvaluations (maybe mxs snd mbex) (maybe mys snd mbey)
```

We start evaluation with the input lists and empty value `Map`s.

```haskell
  in  nextEvaluations (M.empty, xs) (M.empty, ys)
```
