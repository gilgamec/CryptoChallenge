# DSA public-key signature scheme

```haskell
module PublicKey.DSA
  (
    DSAParams(..), genDSAParams
  , DSAKeyPair, DSAPublicKey
  , genDSAKeyPair
  , dsaSign, dsaVerify
  ) where

import Bytes ( HasBytes(..), convBytes )
import Bytes.Integral ()
import Hash ( sha1Hash )
import Modulo ( mkMod, modulo, (^%) )
import PublicKey ( KeyPair(..), PublicKey(..) )
import Random ( randomResidue, randomPrimeR )
import Util ( cdiv )

import Math.NumberTheory.Primes.Testing ( isPrime )

import qualified Control.Monad.Random as R
```

### DSA parameters

DSA has three parameters: a large prime q,
an even larger prime p = 1 + k*q,
and a generator g with multiplicative order q within Zp.

```haskell
data DSAParams = DSAParams{ dsaP, dsaQ, dsaG :: Integer }
  deriving (Eq,Ord,Show)
```

DSA parameter generation isn't actually that difficult,
despite the claims of the Challenge text.

The arguments l and n are the number of bits in p and q, respectively.

```haskell
genDSAParams :: R.MonadRandom m => Int -> Int -> m DSAParams
genDSAParams l n = do
```

q is just a prime number of the right number of bits.

```haskell
  q <- randomPrimeR (2^(n-1), 2^n)
```

Now we have to generate k and p.
We can find the lowest and highest possible values of k.

```haskell
  let kmin = (2^(l-1)) `cdiv` q
      kmax = (2^l) `div` q
```

Now we just generate values of k
until we find one such that 1 + k*q is prime.

```haskell
  let genP = do
        k <- R.getRandomR (kmin,kmax)
        let p = 1 + k*q
        if isPrime p
          then pure (k,p)
          else genP
  (k,p) <- genP
```

Finally, we have to generate g.
We pick a random residue h of p; then h^(p-1) = h^{kq} = 1.
If we set g = h^k, then g^q = 1.
As long as g is not 1, then, its order must be q.

```haskell
  let genG = do
        h <- randomResidue p
        let g = (fromInteger h ^% k) `modulo` p
        if g == 1
          then genG
          else pure g
  g <- genG
  pure $ DSAParams{ dsaP = p, dsaQ = q, dsaG = g }
```

### DSA key generation

The public and private keys are both integers;
the private key is a random integer,
while the public key is g to that power, modulo p.

```haskell
type DSAKeyPair = KeyPair DSAParams Integer Integer
type DSAPublicKey = PublicKey DSAParams Integer
```

Generating a key pair is thus straightforward.

```haskell
genDSAKeyPair :: R.MonadRandom m => DSAParams -> m DSAKeyPair
genDSAKeyPair params = do
  x <- randomResidue (dsaQ params)
  let y = (fromInteger (dsaG params) ^% x) `modulo` dsaP params
  pure KeyPair{ kpParameters = params, kpPrivate = x, kpPublic = y }
```

### DSA message signing

The `dsaSign` function signs the SHA-1 hash of a message.

```haskell
dsaSign :: (HasBytes text, R.MonadRandom m)
        => DSAKeyPair -> text -> m (Integer,Integer)
dsaSign kp text =
  let hash = convBytes (sha1Hash text)
```

We choose a random value k and use it to
compute two integers r and s based on the hash.
However, it is possible that the computed r and s may be zero;
in this case, we have to retry with a different k.

```haskell
      genRS = do
        k <- randomResidue q
        let r = ((mkMod g ^% k) `modulo` p) `mod` q
            s = (mkMod (hash + kpPrivate kp * r) / mkMod k) `modulo` q
        if r == 0 || s == 0
          then genRS
          else pure (r,s)
```

The two returned values r and s then constitute the signature.

```haskell
  in  genRS
 where
  DSAParams{ dsaP = p, dsaQ = q, dsaG = g } = kpParameters kp
```

A valid signature can be verified by the `dsaVerify` function.

```haskell
dsaVerify :: HasBytes text => DSAPublicKey -> text -> (Integer,Integer) -> Bool
dsaVerify pk text (r,s)
```

We first make sure that r and s are in the right range;
that is, 0 < r,s < q.

```haskell
  | r <= 0 || s <= 0 || q <= r || q <= s = False
```

Having confirmed that, we can look at the numbers themselves.

```haskell
  | otherwise =
      let hash = convBytes (sha1Hash text)
```

The signing function computes

    s = k^-1 * (hash + prv * r) (mod q)

(where prv is the private key) so in a valid signature,

    k = s^-1 * hash + s^-1 * prv * r (mod q).

We compute w = s^-1.

```haskell
          w = (recip $ mkMod s) `modulo` q
```

Since g has order q mod p, we can take powers and find that

    g^k = g^(w * hash) g^(w * prv * r)
        = g^(w * hash) (g^prv)^(w * r)

We compute u1 = w * hash and u2 = w * r.

```haskell
          u1 = (w * hash) `mod` q
          u2 = (w * r) `mod` q
```

Now, g^prv is just the public key (mod p), so

    g^k = g^u1 pub^u2  (mod p)

We write the right-hand side as v.

```haskell
          v = (mkMod g ^% u1 * mkMod (pkKey pk) ^% u2) `modulo` p
```

Finally, we take everything mod q to see that

    r = v (mod q)

If this condition holds, the signature is judged valid.

```haskell
      in  r `mod` q == v `mod` q
 where
  DSAParams{ dsaP = p, dsaQ = q, dsaG = g } = pkParameters pk
```
