# Solution to Challenge 59

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Challenge59
  (
    bobEncoderWEC
  , getPrivateKey
  ) where

import Bytes ( HasBytes(..), Bytes )
import Bytes.Integral ()
import EllipticCurve ( WECParameters(..), WECPoint(..)
                     , mkWEC, getWEC, withWEC, withWEC2 )
import PublicKey ( PublicKey(..) )
import PublicKey.ECDiffieHellman ( WECDHParams(..), WECDHKeyPair, WECDHPublicKey
                                 , wecdhSharedSecret )
import Hash ( SHA1MAC, mkHMACSHA1, validateHMACSHA1 )
import Math ( crt )
import Random ( randomWECPoint )
import GroupOps ( pohligHellmanOracle, kangarooChase )

import Control.Monad ( forM )
import Data.List ( mapAccumL )
import Data.Semigroup ( stimes )

import qualified Control.Monad.Random as R
import qualified Data.ByteString as B
```

The shared secret of elliptic curve DH is a point on the elliptic curve.
We say the shared *key* is the concatenation of the x and y coordinates.

```haskell
wecKey :: WECPoint -> Bytes
wecKey WECZero = B.singleton 0
wecKey (WECPoint x y) = toBytes x <> toBytes y
```

The encoder is then straightforward.

```haskell
bobEncoderWEC :: HasBytes message
              => WECDHKeyPair -> message -> WECDHPublicKey -> SHA1MAC message
bobEncoderWEC keypair msg pubkey =
  mkHMACSHA1 (wecKey $ wecdhSharedSecret keypair pubkey) msg
```

The Challenge is to use subgroup confinement on invalid elliptic curves
to find the private key.
This will use the general `pohligHellmanOracle` operation
from the last couple of Challenges, on `WEC` points rather than `MultMod`s.

```haskell
getPrivateKey :: (HasBytes message, R.MonadRandom m)
              => (WECDHPublicKey -> SHA1MAC message) -> WECDHPublicKey
              -> [(WECParameters,Integer)] -> m Integer
getPrivateKey oracle pubKey curves = do
  let PublicKey{ pkParameters = WECDHParams vp g q, pkKey = y } = pubKey
```

We have to make random `WEC`s;
we can just call `mkWEC` on random `WECPoint`s.

```haskell
  let mkRan = fmap mkWEC . randomWECPoint
```

We have to make an oracle of type `WEC p -> Integer -> Bool`
to confirm that we have successfully found the discrete logarithm
of the broken key.

```haskell
  let oracle' w =
        let mac = oracle pubKey{ pkKey = getWEC w }
        in  \k -> validateHMACSHA1 (wecKey (getWEC $ k `stimes` w)) mac
```

Now we can call the Pohlig-Hellman operation for each of our invalid curves.

```haskell
  rms <- forM curves $ \(ivp,o) ->
    withWEC2 ivp (\rng -> pohligHellmanOracle rng oracle' o) (mkRan ivp)
```

rms is a list of pairs (r,m), where r = x (mod m).
We would like to combine them using the Chinese remainder theorem
to find x itself.
Unfortunately, that requires that the ms are relatively prime,
so we have to ensure that first.

If we have some number n, we can make m relatively prime to n
by dividing it by the GCD of the two;
we then have to take r mod this new value.

```haskell
  let reduce n (r,m) = case gcd n m of
        1 -> (n*m, (r,m))
        g -> let m' = m `div` g in (n*m', (r `rem` m', m'))
```

We can then get the final value x by reducing along the list,
then calling `crt`.

```haskell
  let (m,rms') = mapAccumL reduce 1 rms
      r = crt rms'
```

Now we can check if m is at least q; if so, then we are done, and r = x.

```haskell
  if m >= q
    then pure r
```

Otherwise, we can call the kangaroo chase,
exactly as in the previous Challenge.
The categorization function for a point is just the x coordinate.

```haskell
    else let g' = m `stimes` mkWEC g
             y' = mkWEC y <> ((-r) `stimes` mkWEC g)
             cat = fromIntegral . wecX . getWEC
             Just k = withWEC2 vp (\[g',y'] -> kangarooChase cat g' y'
                                                  (0, q `div` m))
                                  [g',y']
         in  pure (r + k*m)
```
