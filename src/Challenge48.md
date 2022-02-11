# Solution to Challenges 47/48

This module implements the entire
[Bleichenbacher padding oracle attack on RSA](https://dx.doi.org/10.1007/BFb0055716).
Although in the Challenges it's split into two (47 and 48),
I think it's just easier to do the whole thing rather than piecemeal.

```haskell
{-# LANGUAGE BangPatterns #-}

module Challenge48
  (
    pkcs1Oracle
  , bb98Attack
  ) where

import Bytes ( HasBytes(..), convBytes )
import Util ( cdiv )
import Interval ( Interval(..), Intervals, mkIntervals, getIntervals
                , intervalsLength, unionAllIntervals, intersectInterval )
import Modulo ( mkMod, modulo, (^%) )
import Padding.PKCS1 ( padPKCS1, validatePKCS1 )
import PublicKey ( KeyPair(..), PublicKey(..), publicPart )
import PublicKey.RSA ( RSAPublicKey, RSAKeyPair, rsaBlockSize, cryptRSA )

import Data.List ( mapAccumL )
import Data.Maybe ( isJust )
```

First up is the padding oracle.
It attempts to decrypt and validate the padding of a provided ciphertext.

```haskell
pkcs1Oracle :: HasBytes ctext => RSAKeyPair -> ctext -> Bool
pkcs1Oracle kp ctext =
  let blockSize = rsaBlockSize (publicPart kp)
      block = cryptRSA (kpPrivate kp) ctext
  in  isJust (validatePKCS1 blockSize block)
```

#### The Bleichenbacher attack

The attack is conceptually similar to Challenge 46, the RSA parity oracle attack.
Valid PKCS1 padding for padding type 2 starts with 0x0002.
We will multiply the ciphtertext by larger and larger numbers s
then check the oracle on the plaintext; if we have valid padding,
then we know that s*p mod n starts with 0x0002, so we can reduce
our range of possibilities for p.

(It is actually very slightly possible (1 chance in 2^64 or so)
that we get a valid type 1 padding instead.
I have no idea how to deal with this;
therefore, this code ignores this (remote) possibility.)

The step numbers and descriptions are taken from
[Bleichenbacher's paper](https://dx.doi.org/10.1007/BFb0055716).

```haskell
bb98Attack :: HasBytes ctext
           => RSAPublicKey -> (Integer -> Bool) -> ctext -> [Integer]
bb98Attack pk oracle ctext =
  let (e,n) = pkKey pk
      c = convBytes ctext
```

* "Step 1: Blinding" is not necessary.
  We start off with a valid ciphertext; thus, our initial bounds
  are between 00:02:00...00 and 00:02:ff..ff.

```haskell
      bee = 2 ^ (8 * (rsaBlockSize pk - 2))
      padBottom = 2 * bee
      padTop = 3 * bee - 1
      initRange = mkIntervals padBottom padTop
```

* "Step 2a: Starting the search".
  We want to find the smallest integer that multiplies the plaintext
  to create another padding-compliant plaintext.
  Note that any integer below (n / padTop)
  will not wrap the plaintext around n even once,
  so we start looking with that value.

```haskell
      initS = nextS [ n `cdiv` padTop .. ]
```

The function `nextS` goes through the provided list
and finds the first s value which,
when multiplied by the plaintext,
creates a plaintext that satisfies the oracle.

```haskell
      nextS = head . filter (oracle . homoMult)
      homoMult s = (fromInteger c * (mkMod s ^% e)) `modulo` n
```

* "Step 2b: Searching with more than one interval left"
  is just moving to the next bigger s, i.e. `nextS [s+1..]`.

* "Step 2c: Searching with one interval left"
  lets us find a new s given a single interval [a,b].

```haskell
      increaseS :: Interval -> Integer -> Integer
      increaseS (Interval a b) lastS =
```

We know that

    padBottom <= s*m mod n <= padTop,

so there is an integer r such that

    padBottom <= s*m - r*n <= padTop.

r is the number of times that we wrap around n
when multiplying by s.
We see then that for a given value of r,

	(r*n + padBottom) / b <= s <= (r*n + padTop) / a.

We can simplify our search for s by just checking these few values
for each instance of r;
we only have to start off with a large enough r.

```haskell ignore
  nextS [ s' | r <- [rStart..]
             , let sStart = (r*n + padBottom) `div` b
             , let sEnd = (r*n + padTop) `cdiv` a
             , s' <- [sStart .. sEnd] ]
```

We don't build the list in this way, however.
This direct construction will contain some s values more than once;
in fact, as many times as there are rs which satisfy

    (r*n + padBottom) / b <= s <= (r*n + padTop) / a.

We instead build the list by carrying along the highest s value reached so far
and starting our list of valid s there.

```haskell
        nextS $ concat . snd $ mapAccumL nextR lastS [rStart..]
       where
        nextR highS r =
          let sStart = max (highS+1) $ (r*n + padBottom) `div` b
              sEnd = (r*n + padTop) `cdiv` a
          in  (sEnd, [sStart..sEnd])
```

Just as in the previous Challenge,
we halve our search space each step by doubling r;
the old r satisfies

    (s*a - padTop) / n <= r <= (s*b - padBottom) / n

so the next r is at least

```haskell
        rStart = 2 * (lastS * b - padBottom) `div` n
```

* "Step 3: Narrowing the set of solutions"
  is how we reduce the interval, given the new value of s.

```haskell
      narrowRange :: Intervals -> Integer -> Intervals
      narrowRange oldRange s =
```

The fact that multiple r values correspond to a single s value
is also why we have to use `Intervals` instead of a single interval (a,b)
like in the last Challenge.
Since we don't actually know what the corresponding value of r is,
i.e. how many times that multiplication wraps around n,
we have to come up with a separate interval for each possibility.
For a given value of s on interval (a,b), we know that

    (s*a - padTop) / n <= r <= (s*b - padBottom) / n;

```haskell
        let rsFor (Interval a b) = [(s*a - padTop) `div` n ..
                                    (s*b - padBottom) `cdiv` n]
```

The interval corresponding to a given value for r is

```haskell
            intervalFor r = mkIntervals ((padBottom + r*n) `cdiv` s)
                                        ((padTop + r*n) `div` s)
```

Then all of the intervals that a given (a,b) fragments into are
just the union of all of these, for all possible r,
intersected with the original interval.

```haskell
            fragments i = intersectInterval i $
                          unionAllIntervals $
                          map intervalFor (rsFor i)
```

Finally, we map this over all of the intervals in the old range
and combine them to get the total possible range for the message.

```haskell
        in  unionAllIntervals $ map fragments $ getIntervals oldRange
```

* "Step 4: Computing the solution"
  is now straightforward.

We create an ever-more-constraining series of interval ranges
for the plaintext by increasing s.

```haskell
      decreasingRanges = iterate nextIter (initS, narrowRange initRange initS)

      nextIter :: (Integer,Intervals) -> (Integer,Intervals)
      nextIter (!s,ivs) =
```

If we have only one constraining interval,
then we can limit the number of possible s values that might pass the oracle;
this is done by `increaseS`.
Otherwise, we just try the next integer in sequence with `nextS`.

```haskell
        let s' = case getIntervals ivs of
                   []  -> error "Range went to empty in bb98Attack.nextIter"
                   [i] -> increaseS i s
                   _   -> nextS [s+1..]
```

Once we have a value of s that multiplies to valid padding,
we narrow the range of our current intervals
and return for the next iteration.

```haskell
        in  (s', narrowRange ivs s')
```

For the answer, we just grab intervals until they shrink to a single value,
then extract their upper bounds.

```haskell
  in  map (hiBound . last . getIntervals) $
      takeUntil ((==1) . intervalsLength) $
      map snd decreasingRanges
```

---

The helper function `takeUntil` is like `takeWhile (not f)`
but also takes the first value for which the predicate is true.

```haskell
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = [x]
  | otherwise = x : takeUntil p xs
```
