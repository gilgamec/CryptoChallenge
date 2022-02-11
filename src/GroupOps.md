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
  , pohligHellman
  , kangarooChase
  , pollardRho
  ) where

import Math ( crt, smallFactors, solveLinMod )

import Control.Monad ( forM, foldM )
import Data.Function ( on )
import Data.Maybe ( listToMaybe )
import Data.Semigroup ( stimes )

import Math.NumberTheory.Logarithms ( integerLog2 )
import Math.NumberTheory.Primes ( unPrime, factorise )

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

---

The normal version of the Pohlig-Hellman algorithm
computes the general discrete logarithm of v with base g.
We use the prime factorization of n to split up the work;
we find the discrete logarithm in each subgroup of prime power order,
then combine them with the Chinese remainder theorem to get
the discrete logarithm in the original group.

`pohligHellman` runs over some `Monoid`;
it needs a hash-like categorization function for that type.

```haskell
pohligHellman :: (Monoid m, Eq m)
              => (m -> Int)
```

The other arguments are then the base of the logarithm
and its order in the group,
and the value whose logarithm we need.

```haskell
              -> m -> Integer -> m
```

Ultimately, it is possible that our log-finder might fail;
we can thus only return `Maybe Integer`.

```haskell
              -> Maybe Integer
pohligHellman cat g n v = crt <$> traverse phPrimePower fs
 where
```

We'll have to grab every odd factor of the order.

```haskell
  fs = filter (odd . unPrime . fst) $ factorise n
```

Every element of fs is a pair (Prime p, e), signifying that p^e is a factor of n.
We want to find the discrete logarithm of v within each subgroup of these sizes;
this is done with the function `phPrimePower`.

```haskell
  phPrimePower (prime,e) =
    let p = unPrime prime
        pe = p^e
```

First, we modify g and v to restrict ourselves to the subgroup
of size p^e.

```haskell
        nSub = n `quot` pe
        g' = nSub `stimes` g
        v' = nSub `stimes` v
```

If we let d = k mod p, then we can write

    v' = g'^k mod p^e.
       = g'^(d + p k') mod p^e.

If we write n' = p^(e-1), then

    v^n' = g^(d n' + n' p k')
	     = g^(d n' + p^e k')
	     = (g^n')^d mod p^e.

We can thus find d = k mod p by finding the logarithm of `v'^n'` wrt `g'^n'`.
And note that once we know d, we see that

    v = g'^(d + pk') = g'^d * (g'^p)^k'

so we set `v'' = v' * g'^(-d)` and `g'' = g'^p` and get

    v'' = g''^k' mod p^(e-1).

We can then recurse with v'' and g'' to find the next 'digit' of k.

This logic is folded into the function `nextDigit`;
it takes the already-computed part k and adds the ith digit to it.

```haskell
        nextDigit k i = do
```

The equation to find digit i, given the already-computed values k, is

    v' * g'^(-k) = (g'^(p^(i+1)))^di

or, lifting to a group of order p,

    (v' * g'^(-k)) ^ (p^(e-1-i)) = (g'^n')^di.

```haskell
          let vi = (p ^ (e-1-i)) `stimes` (((n - k) `stimes` g') <> v')
              gi = (p ^ (e-1)) `stimes` g'
```

We use the Pollard rho algorithm to find the next digit,
as a discrete logarithm modulo p:

```haskell
          di <- pollardRho cat gi p vi
```

Then add the digit to the known values k and continue.

```haskell
          pure (k + (p^i) * di)
```

We find all of the digits by folding `nextDigit` over all digits.

```haskell
    in  do
      k <- foldM nextDigit 0 [0..e-1]
      pure (k, pe)
```

## Pollard's kangaroo chase

Another discrete logarithm solver is
[Pollard's "kangaroo chase"](https://doi.org/10.1090/S0025-5718-1978-0491431-9).

If g is a generator of our group,
we can use it to establish a sequence of elements.
Multiplying an element a0 by g takes us to the "next" element, a1;
multiplying a1 by g takes us to a2, and so on.
The idea behind the kangaroo chase is to "jump" a different distance
at each element;
thus, the jump at a0 might be by 20 steps (multiplying by g^20) to reach a20;
then the jump at a20 might be by 100 steps (multiplying by g^100) to reach a120, and so on.
(The jump sizes are determined from the element, but must be
pseudorandomly distributed.)

Notice that there is a strange effect when we do this.
*From* any element a, there is *only one* element we jump to.
However, there may be *more than one* element that jumps *to* an element b.
Therefore, there must be elements that *no element* jumps to.
In fact, there are quite a few;
on average, about one in three elements have no predecessor,
and this effect compounds: for about one-third of the elements
that have a predecessor, that predecessor itself has no predecessor!
Thus, there are a vanshingly small number of elements that have
a path leading to them a hundred jumps deep.
But this means that if we have taken many jumps,
there are only a small number of places we can be;
and if we instead take many jumps starting from another element,
we are *almost certainly* going to eventually end up in the same place.
(This is also the basis for a
[neat mathematical card trick](http://faculty.uml.edu/rmontenegro/research/kruskal_count/kruskal.html).)

The idea of the "kangaroo chase" is then this:
Our unknown exponent is the "wild kangaroo";
from the initial value we make many pseudorandom jumps.
Then we take a "tame kangaroo" whose exponent we know and
also make pseudorandom jumps.
With high probability, the "wild" value will eventually coincide
with the end point of the "tame" value;
then, as we have recorded the total size of the jumps,
we will be able to easily compute the exponent.

For the kangaroo chase itself, we will need to be able to
generate pseudorandom numbers from general monoidal elements;
we thus need a *categorization function* for these elements.
Something well-distributed and non-correlated, like a good hash function,
is best.

```haskell
kangarooChase :: (Monoid m, Eq m)
              => (m -> Int)
```

We have the base of the logarithm g
and the value v = g^x whose logarithm x we want.

```haskell
              -> m -> m
```

The kangaroo chase is most effective if we have a tight
(but still too large to brute-force) bound on the possible exponent.

```haskell
              -> (Integer,Integer)
```

Even then, we might not be able to find the logarithm.
(If the chase fails, we might try again
after changing the categorization function.)

```haskell
              -> Maybe Integer
kangarooChase s g v (lo,hi) = listToMaybe matches
 where
```

As we are jumping through the group,
we want to keep track of not only our location, but how far we have jumped.
Elements in the sequence will thus be pairs (k,x), where x = x_0 * g^k.

To take a step from (k,x), we evaluate our pseudorandom function f at x;
this gives us our new k_i, which we then add to k
and take as an exponential to multiply by x.
(The `!k` notation means that k must be strict;
this is necessary here or we accumulate an enormous number
of evaluations rather than just computing them as we go.)

```haskell
  kStep (!k,x) = let (ki,gki) = f x in (k + ki , x <> gki)
```

We create two sequences: the 'tame' sequence,
starts from the end of the range at g^hi,
while the 'wild' sequence starts from our query v.

```haskell
  tame = iterate kStep (0,hi `stimes` g)
  wild = iterate kStep (0,v)
```

We want to find a point x where tame meets wild; at this point,

    g^hi * g^kt = x = v * g^kw
     so       v = g^(hi + kt - kw).

Rather than running both in parallel,
we just run the tame one far enough that we can be reasonably sure
it's on the channeled path,
and just check the running wild value against that one point.

```haskell
  (kt,xt) = tame !! n
```

We can turn our known bound on the logarithm
into a bound on how many steps our wild sequence has to take.
Since

    v = g^d = g^(hi + kt - kw),

we can see that `kw = hi + kt - d`, so d > lo means that `kw < hi - lo + kt`.

```haskell
  kwMax = hi - lo + kt
  boundWild = takeWhile ((< kwMax) . fst) wild
```

Now we just look for a match, get the corresponding kw,
and turn it into the logarithm.

```haskell
  matches = [ hi + kt - kw | (kw,xw) <- boundWild, xw == xt ]
```

We have not yet discussed the random-ish steps which turn our
big cyclic group into a graph with a small limit cycle.
After [Pollard 1978](https://doi.org/10.1090/S0025-5718-1978-0491431-9),
we split the group into k pieces [0..k-1],
each corresponding to a step of size 2^k.

```haskell
  fs = V.fromList [ (ki, ki `stimes` g) | i <- [0..k-1], let ki = 2^i ]
```

We simply take the categorization function `mod` k to find the step size
for a given element.

```haskell
  f x = fs V.! (s x `mod` k)
```

The "far enough" number of steps `n` the tame kangaroo takes
should be a constant t times the average step size;
then the probability that we find the logarithm is about `1 - e^(-t)`.
For t = 8, the probability of not finding the value is one third of one percent.
Good enough for me!

```haskell
  n = fromIntegral $ 8 * (2 ^ k) `div` k
```

The only decision left is what k should be. After Pollard:
Supposing m is the average of f(x), and recalling that n = t * m,
we write

    m = a * sqrt (hi - lo),
    n = t * m = t * a * sqrt(hi - lo).

The work done by the algorithm is proportional to the number
of steps taken. This is n for the tame kangaroo;
for the wild kangaroo, it's `(hi - lo)/2m` steps on average to move
from its starting position to the top of the range,
then another n steps to reach the tame kangaroo.
The total number of steps is then about

    steps = 2n + ((hi-lo) / 2m)
          = 2 a t sqrt(hi-lo) + (hi-lo)/2(a sqrt(hi-lo))
          = sqrt(hi-lo) * (2 a t + 1/2a),

which has a minimum value of `2 sqrt(t*(hi-lo))` at `a = 1/(2 sqrt(t))`.
Thus,

             m = (1/2*sqrt(t)) * sqrt(hi-lo)
         2 * m = sqrt(hi - lo) / sqrt(t).

In our case, m = (2^k / k) and t = 8, so

       2 * 2^k = sqrt(hi - lo) * k / sqrt(8)

We take log base 2 to find

           k+1 = (log (hi - lo)) / 2 + log k - (3/2)
             k = (log (hi - lo)) / 2 + (log k - 5/2).

Thus k is approximately half of log (hi - lo). A reasonable (hi - lo)
will be somewhere between 2^20 and 2^50, i.e. k between 10 and 25,
or log k about 4. This makes the second term about 2, so we have

```haskell
  k = (integerLog2 (hi - lo) `div` 2) + 2
```

## Pollard's rho algorithm

Pollard's rho algorithm was introduced at the same time as the kangaroo chase.
If the discrete logarithm's bounds are unknown,
the rho algorithm is more efficient.

Like the kangaroo chase, the function works over any `Monoid`,
and needs a hash-like categorization function.

```haskell
pollardRho :: (Monoid m, Eq m)
           => (m -> Int)
```

The other arguments are the base of the logarithm g, its order in the group,
and the value v whose logarithm we want to find.

```haskell
           -> m -> Integer -> m
```

Our search may fail, so we can return only `Maybe Integer`.

```haskell
           -> Maybe Integer
```

Pollard's rho algorithm operates by constructing a sequence

    x_i = g^a_i * v^b_i,

where the update (a_i,b_i) -> (a_(i+1),b_(i+1)) is pseudorandom based on x.
Just as in the kangaroo chase, this channels almost all values
into a much smaller cycle, generally of size sqrt(n),
so we can search it exhaustively.

```haskell
pollardRho cat g n v = listToMaybe (filter isSolution candidates)
 where
```

We use one of three update functions,
choosing based on the user-provided categorization function:
- (a,b) -> (2a,2b), i.e. x -> x^2;
- (a,b) -> (a+1,b), i.e. x -> x * g; and
- (a,b) -> (a,b+1), i.e. x -> x * v.

```haskell
  newXAB (x,(a,b)) = case cat' x of
    0 -> (x <> x, ((2*a) `mod` n, (2*b) `mod` n))
    1 -> (x <> g, ((a+1) `mod` n, b            ))
    2 -> (x <> v, (a            , (b+1) `mod` n))
```

We require that the categorization function
not assign 0 to the identity object (so `newXAB mempty /= mempty`).
We therefore slightly modify it to make sure it doesn't.

```haskell
  cat' = if cat mempty `mod` 3 == 0
         then \x -> (cat x + 1) `mod` 3
         else \x -> cat x `mod` 3
```

We start our iterations at the identity element and find collisions with the
[Floyd algorithm](https://en.wikipedia.org/wiki/Cycle_detection);
this compares values from a sequence which advances by 1 step
with values from a sequence which advance by 2.

```haskell
  slow = iterate newXAB (mempty,(0,0))
  fast = iterate (newXAB . newXAB) (mempty,(0,0))
```

We then find the (a,b) values for the first collision.

```haskell
  ((_,(ai,bi)),(_,(aj,bj))) = head $ dropWhile (uncurry ((/=) `on` fst)) $
                              tail $ zip slow fast
```

Since we have a collision, we know that

    g^a_i * v^b_i = g^a_j * v^b_j
    g^(a_i - a_j) = v^(b_j - b_i) = g^(k * (b_j - b_i))

so

    (b_j - b_i) * k = (a_i - a_j) mod n.

We just have to solve this linear equation to find the discrete logarithm k.

```haskell
  candidates = solveLinMod n ((bj - bi) `mod` n) ((ai - aj) `mod` n)
```

There might be multiple possible solutions to this linear equation;
we can check whether a particular solution is valid by checking if v = g^k.

```haskell
  isSolution k = v == k `stimes` g
```
