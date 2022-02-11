# Solution to Challenge 46

This Challenge is a warm-up for the Bleichenbacher attack
in Challenges 47 and 48.

```haskell
module Challenge46
  (
    rsaParityOracle
  , rsaParityDecrypt
  ) where

import Bytes ( HasBytes(..), convBytes )
import Util ( cdiv )
import Modulo ( modulo, (^%) )
import PublicKey ( KeyPair(..), PublicKey(..) )
import PublicKey.RSA ( RSAKeyPair, RSAPublicKey, cryptRSA )

import Data.List ( unfoldr )
```

We need an oracle to tell us whether the plaintext corresponding
to a given ciphertext is even or odd.

```haskell
rsaParityOracle :: HasBytes text => RSAKeyPair -> text -> Bool
rsaParityOracle kp = odd . cryptRSA (kpPrivate kp)
```

Using the parity oracle, we can decrypt any message,
one bit at a time.
(It returns a list of results so we can do cool "Hollywood-style" updating. 😎)

```haskell
rsaParityDecrypt :: HasBytes text
                 => RSAPublicKey -> (Integer -> Bool) -> text
                 -> [Integer]
rsaParityDecrypt pk oracle ctext =
  let (e,n) = pkKey pk
      text = convBytes ctext
```

A priori, an unknown message p could be anything between 0 and n-1.
Suppose we multiply the ciphertext by 2^e;
this has the effect of doubling the plaintext. 
If p is small (less than n / 2), then 2p will be less than n,
and it (being 2 times a number) will be even.
If p is larger than n / 2, then 2p will be greater than n, so
the decrypted plaintext (which is mod n) will be 2p - n,
an even number less an odd number, and thus odd.
The parity of (2p mod n) thus tells us whether p is greater or less than n / 2.

Now look at the parity of (4p mod n). We have the same conclusion as before:
if (4p mod n) is even, then (2p mod n) < n / 2,
while if (4p mod n) is odd, then (2p mod n) > n / 2.
But this can be combined with what we already know about p:
if p < n / 2 and (2p mod n) < n / 2, then p < n / 4;
if p > n / 2 and (2p mod n) < n / 2, then n / 2 < p < 3n / 4, and so on.

Each query of the parity of (2^k) p halves the region under consideration,
so we need only log n samples to determine p exactly.
We put this parity query into a function.

```haskell
      isOddFor k = oracle $ (fromInteger text * 2^%(k*e)) `modulo` n
```

The currently-known bounds of the text are a pair (l,h).
We're going to `unfoldr` a function `reduceBounds`,
which uses `isOddFor` to halve the bounds for each new bit k.

```haskell
      reduceBounds k (l,h)
```

If the bounds coincide, we've found the exact message and can return.

```haskell
        | l == h = Nothing
```

Otherwise, what are the exact new bounds we have to use?
We want to find m such that the new bounds
are (l,m-1) if the parity is even, or (m,h) if odd.
(We also increment k and return the current upper bound
as a running estimate.)

```haskell
        | isOddFor k = Just (h, (k+1, (m,h)))
        | otherwise  = Just (m-1, (k+1, (l,m-1)))
       where
```

We know that (l * 2^(k-1)) and (h * 2^(k-1)) wrap n the same number of times,
but (l * 2^k) must wrap one less time than (h * 2^k).
m must be the smallest value such that (m * 2^k) wraps the same
number of times as (h * 2^k), or one more time than (l * 2^k).
The number of wrappings of (l * 2^k) is

```haskell
        d = (l * 2^k) `div` n
```

so we want

    (m * 2^k) `div` n = d + 1
             m * 2^k >= n * (d + 1)
                   m >= (n * (d + 1)) `div` 2^k,

or

```haskell
        m = (n * (d + 1)) `cdiv` (2^k)
```

Finally, we perform the unfold and return the results.

```haskell
  in  unfoldr (uncurry reduceBounds) (1, (0,n-1))
```
