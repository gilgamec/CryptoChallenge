# Miscellaneous mathematical functions

This module contains some useful mathematical functions
used by several of the Challenges.

```haskell
module Math
  (
    crt
  , iroot
  , smallFactors
  ) where

import Util ( xgcd )

import Data.Bifunctor ( first )
import Data.Word ( Word )

import Math.NumberTheory.Primes ( nextPrime, precPrime, unPrime )
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

---

`smallFactors` finds small factors of an integer,
returning the small factors and `Maybe` the unfactored remainder of the number.
(It is a replacement for the function of the same name from the
[arithmoi](https://hackage.haskell.org/package/arithmoi) package,
which has been removed since my first implementation.
It is also undoubtedly not nearly as efficient.)

```haskell
smallFactors :: Int -> Integer -> ([(Integer,Word)], Maybe Integer)
smallFactors pmax = go smallPrimes
 where
```

We first need a list of small primes.
Fortunately, arithmoi can still do this.

```haskell
  primeBound = fromIntegral pmax
  smallPrimes = map unPrime [ nextPrime 2 .. precPrime primeBound ]
```

Given a number and a prime,
we use `divAll` to divide out all multiples of it.

```haskell
  divAll (p,k) n = case n `quotRem` p of
    (q,0) -> divAll (p,k+1) q
    _     -> ((p,k),n)
```

Most of the work is now done by `go`, which takes a list of small primes
and does trial division on them in sequence.

```haskell
  go (p:ps) m
```

We know that m has no factors less than p;
therefore, if m is less than p^2, it must be prime itself.
It goes in the list of factors if it is small enough.

```haskell
    | m < p*p = if m <= primeBound
                then ([(m,1)],Nothing)
                else ([], Just m)
```

Otherwise, we check if m is divisible by p
and divide out any factors it has, recursing.

```haskell
    | otherwise = case divAll (p,0) m of
        ((_,0),_) -> go ps m
        (pk,m')   -> first (pk:) $ go ps m'
```

If the list of primes is empty, then m has no factors less than `primeBound`.
It therefore goes in the second return value.

```haskell
  go [] m = ([], Just m)
```
