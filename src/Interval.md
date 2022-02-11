# Interval handling

This module implements basic interval functions,
including unions and intersections.
(At the time I completed these Challenges,
I couldn't find an existing package to do this.)

```haskell
module Interval
  (
    Interval(..)
  , Intervals(..), mkIntervals
  , intervalsLength
  , unionIntervals, unionAllIntervals
  , intersectInterval
  ) where

import Data.List ( foldl' )
```

A single interval has a low and high bound;
it represents an `Integer` x such that

    loBound <= x <= hiBound.

```haskell
data Interval = Interval { loBound, hiBound :: !Integer }
  deriving (Eq,Ord,Show)
```

We mostly deal with lists of disjoint intervals.
They are stored in the list sorted from left- to rightmost:

```haskell
newtype Intervals = Intervals { getIntervals :: [Interval] }
  deriving (Eq,Ord,Show)
```

The smart constructor makes sure that the lower and upper bounds
are assigned correctly.

```haskell
mkIntervals :: Integer -> Integer -> Intervals
mkIntervals lo hi
  | lo > hi   = Intervals []
  | otherwise = Intervals [ Interval lo hi ]
```

`intervalsLength` reports the length of the collection of intervals;
it's just the sum of the lengths of the individual intervals.

```haskell
intervalsLength :: Intervals -> Integer
intervalsLength (Intervals ivs) = sum $ map ilength ivs
 where
  ilength (Interval lo hi) = hi - lo + 1
```

The union of two intervals is either one (if they overlap)
or two (if disjoint) intervals.

```haskell
unionItv :: Interval -> Interval -> Intervals
unionItv ix@(Interval lx hx) iy@(Interval ly hy)
```

We first ensure that the first interval is the leftmost.

```haskell
  | ly < lx   = unionItv iy ix
```

We now check if the lower bound of the rightmost interval
is at least two above the upper bound of the leftmost;
in this case, the intervals are distinct and we return them both.
The two-above condition is necessary, because for integer intervals,

    a <= x <= b
	b+1 <= x <= c

means that the union is the single interval

    a <= x <= c.

```haskell
  | hx+1 < ly = Intervals [ix,iy]
```

Otherwise, we have a single interval running from
the lowest of the low to the highest upper bound.

```haskell
  | otherwise = mkIntervals lx (max hx hy)
```

To take the union of lists of intervals,
we have to combine them pairwise and union them as needed.

```haskell
unionIntervals :: Intervals -> Intervals -> Intervals
unionIntervals (Intervals []) ys = ys
unionIntervals xs (Intervals []) = xs
```

We combine non-empty interval lists with a merge function `go`.

```haskell
unionIntervals (Intervals (ix:xs)) (Intervals (iy:ys))
  | ix < iy   = Intervals $ go ix xs (iy:ys)
  | otherwise = Intervals $ go iy (ix:xs) ys
 where
```

The `go` function maintains an active interval `a`.
At each invocation, it finds the next lowest interval from the input lists
and calls `combine` to join it to the active interval.
If there are no more intervals left to add, then we're done.

```haskell
  go a [] [] = [a]
```

We want to be pulling from the first list.

```haskell
  go a [] ys = go a ys []
```

If the second list is empty, then we combine our active interval
with what's left of the first list.

```haskell
  go a (x:xs) [] = combine a x xs []
```

Otherwise, we pick the leftmost interval of the two available
and combine with it.

```haskell
  go a (x:xs) (y:ys) = case compare x y of
    LT -> combine a x xs (y:ys)
    EQ -> combine a x xs ys
    GT -> combine a y (x:xs) ys
```

`combine` handles combining the two leftmost intervals we are currently working on.

```haskell
  combine i1 i2 xs ys = case unionItv i1 i2 of
```

If i1 and i2 are disjoint,
then the leftmost must be disjoint with any of the remaining intervals,
and we continue with the rightmost being active.

```haskell
    Intervals [j0,j1] -> j0 : go j1 xs ys
```
 
If they are not disjoint, then we continue with the merged interval active.

```haskell
    Intervals [j1]    ->      go j1 xs ys
```

The function `unionAllIntervals`
performs a fold to take the union of a number of intervals.

```haskell
unionAllIntervals :: [Intervals] -> Intervals
unionAllIntervals = foldl' unionIntervals (Intervals [])
```

The intersection of two intervals is either one (if they overlap)
or zero (if disjoint) intervals.

```haskell
intersectItv :: Interval -> Interval -> Intervals
intersectItv ix@(Interval lx hx) iy@(Interval ly hy)
```

We first ensure that the first interval is the leftmost.

```haskell
  | ly < lx   = intersectItv iy ix
```

If the lower bound of the rightmost interval
is above the upper bound of the leftmost,
the intervals are distinct and we return an empty list.

```haskell
  | hx < ly   = Intervals []
```

Otherwise, we have a single interval running from
the highest of the low to the lowest upper bound.

```haskell
  | otherwise = mkIntervals ly (min hx hy)
```

We will only need to intersect a list of `Intervals` with a single `Interval`.
This can be done by just mapping `intersectItv` onto all sub-intervals.

```haskell
intersectInterval :: Interval -> Intervals -> Intervals
intersectInterval isect (Intervals ivs) = Intervals
  [ iv' | iv <- ivs, iv' <- getIntervals (intersectItv isect iv) ]
```
