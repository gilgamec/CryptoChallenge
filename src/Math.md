# Miscellaneous mathematical functions

This module contains some useful mathematical functions
used by several of the Challenges.

```haskell
module Math
  (
    crt
  , iroot
  , smallFactors
  , modSqrt
  ) where

import Modulo ( mkMod, modulo, (^%) )
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

---

Square root modulo p.
This is straight from the
[Wikipedia page](https://en.wikipedia.org/wiki/Tonelli%E2%80%93Shanks_algorithm).

```haskell
modSqrt :: Integer -> Integer -> Maybe Integer
modSqrt p n
```

If n = 0 mod p, then its square root is just zero.

```haskell
  | n `mod` p == 0 = Just 0
```

It's easy to check if a number is indeed a perfect square
(a 'quadratic residue').
If the number is not a quadratic residue, it has no square root.

```haskell
  | not (isQR n)   = Nothing
```

Otherwise, we're going to run the Tonelli-Shanks loop
and take the answer modulo p.

```haskell
  | otherwise      = Just $ loop r0 t0 m0 c0 `rem` p
 where
```

For any residue k, k^(p-1) = 1 mod p.
Therefore, if k = x^2, then k^((p-1)/2) = 1 mod p.

```haskell
  halfP = (p-1) `quot` 2
  isQR k = (mkMod k ^% halfP) `modulo` p == 1
```

We factor all powers of 2 out of p-1, giving us p-1 = q * 2^s.

```haskell
  (s,q) = fac2 (0,p-1)
  fac2 (k,n) = case n `quotRem` 2 of
    (q,0) -> fac2 (k+1,q)
    _     -> (k,n)
```

The main loop repeatedly tries potential roots,
starting with r = n^((q+1)/2).
For this value, r^2 = n * n^q, and r = sqrt n iff t = n^q = 1.
Since q is the entire odd part of the order of the group,
t must be a (2^i)-th root of 1, i.e. t^(2^i) = 1, for some i <= s.
Our goal is to factor squares out of t and put them into r, i.e.

    r -> r*b, t -> t*b^2.

It turns out that the powers of z, a non-quadratic-residue,
are the squares we need.

```haskell
  z = head $ dropWhile isQR [ 2 .. p-1 ]
```

The loop has parameters

- r, the root so far; r^2 = n * t.
- t, the squares remaining to factor into r;
- m, the number of factors of 2 left to account for;
- c, a power of z^q, with order 2^m.

First, we repeatedly square t until it reaches 1,
i.e. we find i such that t^(2^i) = 1.
If t is already 1 (i.e. i is zero), then we're done and just return r.

```haskell
  modSq x = (x * x) `rem` p
  loop r t m c = case length $ takeWhile (/=1) $ iterate modSq t of
    0 -> r
```

Otherwise, our factor b must be c^(2^(m-i-1)) and we can recurse.

```haskell
    i -> let b = (mkMod c ^% (2^(m-i-1))) `modulo` p
             b2 = (b*b) `rem` p
         in  loop ((r*b) `rem` p) ((t*b2) `rem` p) i b2
```

We can then run the loop starting with

```haskell
  r0 = (mkMod n ^% ((q+1) `div` 2)) `modulo` p
  t0 = (mkMod n ^% q) `modulo` p
  m0 = s
  c0 = (mkMod z ^% q) `modulo` p
```
