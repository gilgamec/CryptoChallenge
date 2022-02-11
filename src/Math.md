# Miscellaneous mathematical functions

This module contains some useful mathematical functions
used by several of the Challenges.

```haskell
module Math
  (
    crt
  , iroot
  ) where

import Util ( xgcd )
```

`crt` constructs the congruence described by the Chinese remainder theorem.
If

    n = a1 mod x1
    n = a2 mod x2
       ...
    n = ak mod xk

then `crt` can be used to compute a number congruent to
n mod (x1 * x2 * ... * xk), providing the xs are relatively prime.

The argument is a list of pairs (ai,xi).

```haskell
crt :: [(Integer,Integer)] -> Integer
```

The function proceeds through the list element by element,
combining each pair of elements in turn.

```haskell
crt ((a1,n1) : (a2,n2) : ans) =
```

We use the extended Euclidean algorithm
to find numbers (m1,m2) such that

    m1*n1 + m2*n2 = 1.
	
```haskell
  let (1, (m1,m2)) = xgcd n1 n2
```

Note that `m2*n2 = 1 (mod n1)` and vice versa, so the number

```haskell
      x = (a1*n2*m2 + a2*n1*m1) `mod` (n1*n2)
```

is equal to a1 (mod n1) and a2 (mod n2);
this is the combined number we're looking for.

We then recurse to continue along the list.

```haskell
  in  crt ((x, n1*n2) : ans)
```

When we come down to a single element, it is the answer.

```haskell
crt [(a,n)] = a
```

---

`iroot` computes the floor of the square root of an integer.
It uses a Newton-Raphson iteration to find the root.

```haskell
iroot :: Integral a => a -> a -> a
iroot r n =
```

We want to find the zero of the function

    f(x) = x^r - n

with derivative

    f'(x) = r x^(r-1)

The iteration is then

    x <- x - f(x) / f'(x)
	  = x - (x^r - n) / (r x^(r-1))
      = (r x^r - x^r + n) / (r x^(r-1))
      = (1/r) ((r-1) x + n / x^(r-1))

```haskell
  let r1 = r - 1
      xs = iterate (\x -> (x * r1 + n `div` (x ^ r1)) `div` r) 1
```

For some (most?) values of n,
the sequence converges to the floor of the root,
so we only have to look for repeated values in the list.
Occasionally, though, the sequence will alternate
between the two integers around the root.
(See e.g. the cube root of 200, whose iteration alternates between 5 and 6.)
We therefore check each element against the next two elements in the list.

```haskell
  in  findAns xs
 where
  findAns (x:xs@(y:z:_))
    | x == y || x == z = min y z
    | otherwise = findAns xs
```
