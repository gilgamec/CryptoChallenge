# Solution to Challenge 45

```haskell
module Challenge45
  (
    dsaZeroG, dsaZeroGForge
  , dsaP1G, dsaP1GForge
  ) where

import Modulo ( mkMod, modulo, (^%) )
import PublicKey ( PublicKey(..) )
import PublicKey.DSA ( DSAParams(..), DSAPublicKey )
```

The first set of bad parameters has g = 0.

```haskell
dsaZeroG :: DSAParams -> DSAParams
dsaZeroG params = params{ dsaG = 0 }
```

During validation, we create the value

    v = g^(s^-1 * hash) pubkey^(s^-1 * r) (mod p).

Validation succeeds if v == r, so if g is zero then
any string will theoretically validate with a signature with r = 0.

Unfortunately (or, I suppose, fortunately),
the DSA validation algorithm will fail if r is not in the range 0 < r < q,
so this forgery is a no-go.

```haskell
dsaZeroGForge :: DSAPublicKey -> (Integer,Integer)
dsaZeroGForge _ = (0,12345678)
```

The second set of parameters has g = p+1.

```haskell
dsaP1G :: DSAParams -> DSAParams
dsaP1G params = params{ dsaG = dsaP params + 1 }
```

If g is (p+1), then

    v = pubkey^(s^-1 * r),

so we can sign any message with

    (r,s) = (pubkey^z, r / z)

for any z.

```haskell
dsaP1GForge :: DSAPublicKey -> (Integer,Integer)
dsaP1GForge pk = (r,s)
 where
  DSAParams{ dsaP = p, dsaQ = q, dsaG = g } = pkParameters pk
  z = 12345678
  r = ((mkMod (pkKey pk) ^% z) `modulo` p) `mod` q
  s = (mkMod r / mkMod z) `modulo` q
```
