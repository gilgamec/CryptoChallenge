# Solution to Challenge 62

```haskell
module Challenge62
  (
    wecdsaSignBroken
  , breakWeakWECDSA
  ) where

import Bytes ( HasBytes(..), convBytes )
import Random ( randomResidue )
import EllipticCurve ( WECPoint(..), mkWEC, modWEC )
import PublicKey ( KeyPair(..), PublicKey(..) )
import PublicKey.ECDSA ( WECDSAParams(..), WECDSAKeyPair, WECDSAPublicKey )
import Modulo ( mkMod, modulo )
import Hash ( sha1Hash )
import LLL ( RowVec(..), lll )

import Control.Monad ( replicateM )
import Data.Bits ( Bits(..) )
import Data.Maybe ( listToMaybe )
import Data.Semigroup ( stimes )

import qualified Control.Monad.Random as R
import qualified Data.Vector as V
```

We implement a broken WECDSA signature that zeros out the last bits of k.

```haskell
wecdsaSignBroken :: (HasBytes text, R.MonadRandom m)
                 => Int -> WECDSAKeyPair -> text -> m (Integer,Integer)
wecdsaSignBroken bits kp text = genRS
 where
  hash = convBytes (sha1Hash text)
  WECDSAParams{ wecdsaParams = ps
              , wecdsaQ = q, wecdsaG = g } = kpParameters kp
```

Zeroing out the last bits just involves ANDing against the appropriate mask.
(Since `Integer` is unbounded, we have to apply the mask in a roundabout way.)

```haskell
  zeroMask = (2^bits) - 1
  zeroLastBits n = n `xor` (n .&. zeroMask)
```

Generating the signature is then a straightforward extension.

```haskell
  genRS = do
    k <- zeroLastBits <$> randomResidue q
    let r = wecX ((k `stimes` mkWEC g) `modWEC` ps) `rem` q
        s = (mkMod (hash + kpPrivate kp * r) / mkMod k) `modulo` q
    if r == 0 || s == 0
      then genRS
      else pure (r,s)
```

---

We can find the private key from this broken signature scheme
using lattice basis reduction.
Look at the WECDSA signing algorithm. The signature s is

    s = ((m + d * r) / k) mod q

where m is the mesage hash, r is effectively g^k, and d is the private key.
If the lowest l bits of k are zero, then k = b * 2^l and

          s = (m + d * r) / (b * 2^l)
    s * 2^l = (m + d * r) / b
          b = m / (s * 2^l) + (d * r) / (s * 2^l)
            = u + d * t

where u = m / (s * 2^l) and t = r / (s * 2^l).
This is all mod q, so b = u + d * t + n * q for some n.
We know q; m, r, s, and l (therefore u and t);
and d is constant, so multiple signatures give us linear equations

    bi = ui + d * ti + ni * q;

the unknowns are bi, d, and ni. We therefore get equations

    q n0 +    0 + ... +    0 + t0 d + u0 = b0
           q n1 + ... +    0 + t1 d + u1 = b1
                  ...
                        q nk + tk d + uk = bk

whose sum is `sum bi`.

Look at the lattice spanned by the columns of the system,

    { e0 = (q,0,..,0), e1 = (0,q,..,0), ...
	, ek = (0,0,..,q), et = (t0,t1,..,tk), eu = (u0,u1,..,uk) };

the bi are on the order of q / 2^l, much smaller than u or d,
so we know that

    n0*e0 + n1*e1 + ... + nk*ek + d*et + eu = sum bi

is small, and hopefully if we do a basis reduction we'll get that
as one of the elements.
To grab the actual value d, we add on two more dimensions
(this also makes the system square, i.e. a basis)
which only have values for et and eu:

    { (q,0,..,0,0,0), (0,q,..,0,0,0), ..., (0,0,..,q,0,0)
	, (t0,t1,..,tk,ct,0), (u0,u1,..,uk,0,cu) };

Since the small vector has 1 for the the coefficient of eu,
we look for cu in the last place in our new basis.
If one of the vectors has it, then often its next-to-last place
will be d * ct, and we can divide to get d.

The function itself takes an oracle which returns messages
signed with a biased nonce.
It calls the oracle repeatedly until it is able to isolate the private key,
then returns it.

```haskell
breakWeakWECDSA :: (R.MonadRandom m, HasBytes text)
                => Int -> WECDSAPublicKey -> m (text,(Integer,Integer))
                -> m Integer
breakWeakWECDSA bits pk getMsg = moreMessages []
 where
  WECDSAParams{ wecdsaParams = params
              , wecdsaQ = q, wecdsaG = g } = pkParameters pk
```

The main loop queries the oracle for
more signed messages and tries to compute the key.

```haskell
  moreMessages msgs = do
```

We read five more messages per LLL invocation.

```haskell
    msgs' <- (++msgs) <$> replicateM 5 getMsg
```

We could probably reduce the number of operations we have to do
by reusing our old reduced matrix and only adding new rows and columns;
however, for simplicity, we just recompute every time.

```haskell
    case getPrivateKey msgs' of
```

If we couldn't isolate the private key from those messages,
we get some more and try again.

```haskell
      Nothing -> moreMessages msgs'
```

Otherwise, we return the key.

```haskell
      Just pk -> pure pk
```

Given a list of messages signed with weak ECDSA,
we can construct the column space of the linear system.

```haskell
  columnSpace msgs =
    let n = length msgs
```

The Challenge text recommends that our sentinel values be divided by 2^l;
rather than do that (and use a lattice over `Rational`s)
I'll just multiply the other values by 2^l.

```haskell
        twol = 2^bits
```

The first n rows just have a single (q * 2^l) entry
in each of the last n elements.
This can be easily built by taking slices of a single vector.

```haskell
        qvec = V.replicate (n+1) 0 <> V.singleton (q * twol) <>
               V.replicate (n-1) 0
        qs = [ V.take (n+2) vec | i <- [0..n-1], let vec = V.drop i qvec ]
```

Next is the `t` row. The first two elements are sentinels `[0,1]`;
the rest are the values

    t_i = 2^l * (r / (s * 2^l)) mod q

```haskell
        ts = V.fromListN (n+2) $
             0 : 1 : [ twol * (mkMod r / mkMod (s * twol)) `modulo` q
                     | (_,(r,s)) <- msgs ]
```

Finally, the `u` row has sentinels `[q,0]` and values

    u_i = 2^l * (h / (s * 2^l)) mod q,

where h is the hash of the corresponding message.

```haskell
        us = V.fromListN (n+2) $
             q : 0 : [ twol * (mkMod h / mkMod (s * twol)) `modulo` q
                     | (m,(_,s)) <- msgs
                     , let h = convBytes (sha1Hash m) :: Integer ]
```

The column space is then

```haskell
    in  us : ts : qs
```

Given a number of signed messages,
we can now generate and test candidate private keys.

```haskell
  getPrivateKey msgs =
```

We generate our column space and call the LLL algorithm to reduce the basis.

```haskell
    let reduced = map unRowVec . lll . map RowVec $ columnSpace msgs
```

Now we only have to look at the sentinel elements,
which are the first two elements of each row.
If the first element is q, then it is likely that
the second will be the private key;
it is also possible that the first element will be -q,
in which case the second will likely be the negative of the private key.
We try to find the true private key from these candidates.

```haskell
    in  listToMaybe [ key | row <- reduced
                          , abs (V.head row) == q
                          , let key = (signum (V.head row) * (row V.! 1)) `mod` q
```

To check if we have the correct private key,
we see if we can regenerate the public key.

```haskell
                          , let pubKey = (key `stimes` mkWEC g) `modWEC` params
                          , pubKey == pkKey pk ]
```
