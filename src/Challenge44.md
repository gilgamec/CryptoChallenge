# Solution to Challenge 44

```haskell
module Challenge44
  (
    dsaBreakRepeatedK
  ) where

import Bytes ( HasBytes(..), convBytes )
import Modulo ( mkMod, modulo )
import PublicKey ( PublicKey(..) )
import PublicKey.DSA ( DSAPublicKey, DSAParams(..) )

import Data.Function ( on )
import Data.List ( groupBy, sortBy )
import Data.Maybe ( listToMaybe )
```

If two DSA signatures use the same nonce k,
then they will have the same r;
it is thus simple to detect them.
Once we have two signatures with the same k,
we can recover k and thus the private key.

All we need is a list of hashed messages and their signatures.

```haskell
dsaBreakRepeatedK :: HasBytes digest
                  => DSAPublicKey -> [(digest,(Integer,Integer))]
                  -> Maybe Integer
dsaBreakRepeatedK pk msgs =
  let q = dsaQ (pkParameters pk)
```

We first group the input messages by r values (and thus by nonce):

```haskell
      byNonce = groupBy ((==) `on` (fst . snd)) $
                sortBy (compare `on` (fst . snd)) msgs
```

We create a list associating each unique r value
with the digest and s values of its first two uses.

```haskell
      reps = [ (r,(h1,s1),(h2,s2))
             | (d1,(r,s1)) : (d2,(_,s2)) : _ <- byNonce
             , let h1 = convBytes d1
             , let h2 = convBytes d2 ]
```

A little algebra on the signing algorithm leads us to

    private key * r = hash - k * s (mod q);

since the left-hand side is the same for both messages, we see that

    h1 - k * s1 = h2 - k * s2  (mod q)

so

    k = (h1 - h2) / (s1 - s2)  (mod q).

```haskell
      kFor (h1,s1) (h2,s2) = (mkMod (h1 - h2) / mkMod (s1 - s2)) `modulo` q
```

We know from the last Challenge how to compute the
private key for a given k:

```haskell
      prvFor k r h s = (mkMod (s * k - h) / mkMod r) `modulo` q
```

We can combine these to get the private key from an
element of `reps`.

```haskell
      getPrv (r,(h1,s1),(h2,s2)) = prvFor (kFor (h1,s1) (h2,s2)) r h1 s1
```

Finally, we run this over all of our repeated elements
to get the private key.

```haskell
  in  listToMaybe (map getPrv reps)
```
