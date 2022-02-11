# Solution to Challenge 60

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Challenge60
  (
    bobEncoderMEC
  , vsForU
  , getPrivateKey
  ) where

import Bytes ( HasBytes(..) )
import Bytes.Integral ()
import EllipticCurve ( WECParameters(..), WECPoint(..), mkWEC, withWEC2, getWEC
                     , MECParameters(..), MECPoint, mecLadder )
import PublicKey ( PublicKey(..) )
import PublicKey.ECDiffieHellman ( MECDHParams(..), MECDHKeyPair, MECDHPublicKey
                                 , mecdhSharedSecret)
import Hash ( SHA1MAC, mkHMACSHA1, validateHMACSHA1 )
import Math ( crt, smallFactors, modSqrt )
import Modulo ( mkMod, modulo )
import Random ( randomResidue )
import GroupOps ( kangarooChase )

import Control.Monad ( foldM, forM )
import Data.List ( nub )
import Data.Maybe ( catMaybes )
import Data.Semigroup ( stimes )

import qualified Control.Monad.Random as R
```

The encoder is simpler again because the shared secret is just an integer.

```haskell
bobEncoderMEC :: HasBytes message
              => MECDHKeyPair -> message -> MECDHPublicKey -> SHA1MAC message
bobEncoderMEC keypair msg pubkey =
  mkHMACSHA1 (mecdhSharedSecret keypair pubkey) msg
```

`vsForU` returns the possible v values for a given u.

```haskell
vsForU :: MECParameters -> Integer -> [Integer]
vsForU MECParameters{mecA=a,mecB=b,mecP=p} u =
```

v^2 is computed directly from the elliptic curve equation,

    b v^2 = u^3 + a u^2 + u

```haskell
  let v2 = (let mu = mkMod u
            in  ((mu + mkMod a)*mu*mu + mu) / mkMod b) `modulo` p
```

Then it either has no square root, or two.

```haskell
  in case modSqrt p v2 of
       Nothing -> []
       Just v  -> [v,p-v]
```

We don't have a good arbitrary-element multiplication 
for Montgomery curve points, so we can't implement Monoid
and much of our existing machinery won't work.
We're going to have to recreate some of it here.

`lowOrderTwistBase` finds a base of low order on the twist.

```haskell
lowOrderTwistBase :: R.MonadRandom m
                  => MECParameters -> Integer -> Integer -> m MECPoint
lowOrderTwistBase ps o r =
```

To create an element of the given base, we pick a random u,
making sure it's on the twist by checking it has no corresponding v,
then taking its power and ensuring that it has the required order.

```haskell
  let mkBase k = do
        u <- randomResidue (mecP ps)
        case vsForU ps u of
          _:_ -> mkBase k
          []  ->
            let g = mecLadder ps k u
            in  if g == 1
                then mkBase k
                else pure g
```

We then call `mkBase` with the order divided by the required order.

```haskell
 in case o `quotRem` r of
     (k,0) -> mkBase k
     _     -> error $ "lowOrderTwistBase: target order "++show r++
                      " does not divide group order "++show o
```

`smallDLog` finds all of the elements in the input list
which correspond to the private key mod r.
In the Montgomery formulation, there will be two such: k and r-k.
The function is called in two ways: first, with all numbers less than r
(essentially used to brute-force the key); but also with only a few numbers,
used to narrow down a number of possibilities from the Chinese remainder theorem.

```haskell
smallDLog :: R.MonadRandom m
          => (MECPoint -> Integer -> Bool) -> MECDHPublicKey
          -> Integer -> Integer -> [Integer] -> m [Integer]
smallDLog oracle pubKey o r ks = do
  let PublicKey{ pkParameters = MECDHParams ps _ _ } = pubKey
  h <- lowOrderTwistBase ps o r
  pure $ filter (oracle h) ks
```

`phOracleTwist` performs the same function as the
Pohlig-Hellman-with-oracle function in `GroupOps`,
but working on the twist of a Montgomery curve.
It works much the same way as that function,
except that there are two possible values for the private key mod n.

```haskell
phOracleTwist :: R.MonadRandom m
              => (MECPoint -> Integer -> Bool) -> MECDHPublicKey -> Integer
              -> m ([Integer],Integer)
phOracleTwist oracle pubKey o = do
  let PublicKey{ pkParameters = MECDHParams params _ _ } = pubKey
```

There are 2p+2 points between the original curve and its twist,
so the order of the twist curve is just

```haskell
  let twistOrder = (2 * mecP params + 2) - o
```

We factor the twist order:

```haskell
  let primeBound = 2^24
      rs = [ fac | (fac,1) <- fst $ smallFactors primeBound twistOrder ]
```

For each factor, we call `smallDLog` to brute-force the key modulo that factor.

```haskell
  kss <- forM rs $ \r -> do
    xs <- smallDLog oracle pubKey twistOrder r [0..r-1]
    let x = case xs of
              [] -> error $ "brute force failed for r = "++show r
              x:_ -> x
```

There are two elements of the input list which correspond to the key;
once we know x, we know the other, -x.

```haskell
    pure [ x `rem` r, -x `mod` r ]
```

We combine the possibilities for each modulus by taking the `crt` of each pair;
this gives us up to four possibilities, so we call `smallDLog` with them
to narrow down to only the ones which match the private key.

```haskell
  let crt' (ks1,r1) (ks2,r2) = do
        let ks = [ crt [(k1,r1),(k2,r2)] | k1 <- ks1, k2 <- ks2 ]
            r12 = r1 * r2
        ks' <- nub <$> smallDLog oracle pubKey twistOrder r12 ks
        pure (ks', r12)
```

We can just fold `crt'` over the keys to get our final value.

```haskell
  let kr:krs = zip kss rs
  foldM crt' kr krs
```

Now for the attack itself.
Besides the oracle and the public key,
we will also need the related Weierstrass curve
and a function to convert to points on that curve.

```haskell
getPrivateKey :: (R.MonadRandom m, HasBytes message)
              => (MECDHPublicKey -> SHA1MAC message) -> MECDHPublicKey -> Integer
              -> WECParameters -> (MECPoint -> [WECPoint])
              -> m Integer
getPrivateKey oracle pubKey o wps m2ws = do
  let PublicKey{ pkParameters = MECDHParams params g q, pkKey = y } = pubKey
```

We have to make an oracle of type `MECPoint -> Integer -> Bool`
to send to `phOracleTwist`.

```haskell
  let oracle' m =
        let mac = oracle pubKey{ pkKey = m }
        in  \k -> validateHMACSHA1 (mecLadder params k m) mac
```

We first pull as much information as possible from the twist curve.
This returns two possible values for (private mod r).

```haskell
  (ks,r) <- phOracleTwist oracle' pubKey o
```

We can use the kangaroo chase to narrow this down to the actual value,
but we have to do that on the corresponding WEC curve.
The function `chase` runs the kangaroo chase for the given WEC points.

```haskell
  let kcBounds = (0, q `div` r)
  let chase wg wy k =
        let g' = r `stimes` mkWEC wg
            y' = mkWEC wy <> ((-k) `stimes` mkWEC wg)
            cat = fromIntegral . wecX . getWEC
            kc = withWEC2 wps (\[g',y'] -> kangarooChase cat g' y' kcBounds)
                              [g',y']
        in  (\m -> k + m*r) <$> kc
```

There are, however, two possible values for the base
and two for the public key corresponding to the particular Montgomery points;
these will give the same answer if we get the correct or wrong sign for both,
so we will end up having to check two possibilities for each choice of k,
for up to four possible runs of the kangaroo chase.

```haskell
  pure $ head $ catMaybes [ chase wg wy k
                          | wg <- m2ws g
                          , k <- ks
                          , wy <- m2ws y ]
```
