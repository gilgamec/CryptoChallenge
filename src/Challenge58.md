# Solution to Challenge58

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

module Challenge58
  (
    getPrivateKey
  ) where

import Bytes ( HasBytes(..) )
import Hash ( SHA1MAC, mkHMACSHA1, validateHMACSHA1 )
import PublicKey ( PublicKey(..) )
import PublicKey.DiffieHellman ( DHParams(..), DHPublicKey )
import Modulo ( mkMod, getVal, modulo, withMod, withMod2, (^%)
              , MultMod(..), mkMultMod )
import GroupOps ( pohligHellmanOracle, kangarooChase )
import Random ( randomResidue )

import qualified Control.Monad.Random as R
```

This is very similar to the previous Challenge,
except the prime factors of p are not quite so accomodating
as to get us up to x mod q.
We can, however, use the same Pohlig-Hellman solver
to get x mod r, for some r < q,
then the kangaroo chase to get us the actual value x.

```haskell
getPrivateKey :: (HasBytes message, R.MonadRandom m)
              => (DHPublicKey -> SHA1MAC message) -> DHPublicKey
              -> Integer -> m Integer
getPrivateKey oracle pubKey q = do
  let PublicKey{ pkParameters = DHParams p g, pkKey = y } = pubKey
      mkRan = mkMultMod <$> randomResidue p
      oracle' (MultMod m) =
        let mac = oracle pubKey{ pkKey = getVal m }
        in  \k -> validateHMACSHA1 (getVal (m ^% k)) mac
```

We run the Pohlig-Hellman solver to get n = x mod r.

```haskell
  (n,r) <- withMod2 p (\rng -> pohligHellmanOracle rng oracle' (p-1)) mkRan
```

If we have enough factors, then this will cover q and we can just return.

```haskell
  pure $ if r >= q
    then n `rem` q
```

Otherwise, say n = x mod r; then x = n + mr for some m in [0,q/r].
We can use the kangaroo chase to find m, given these bounds.
We'll need the public key y; then we know that

    y = g^x = g^(n+mr) = g^n (g^r)^m

so we can find m by solving the discrete-log problem

    y' = g^-n y = (g^r)^m = g'^m.

```haskell
    else let g' = mkMod g ^% r
             y' = mkMod y * mkMod g ^% (-n)
```

For the categorization function,
we just use the integer value of the residue,
which is completely scrambled by g.

```haskell
             cat = fromIntegral . getVal . getMultMod
             Just m = withMod2 p (\[g',y'] -> kangarooChase cat
                                              (MultMod g')
                                              (MultMod y')
                                              (0, q `div` r))
                                 [g',y']
         in  n + m*r
```
