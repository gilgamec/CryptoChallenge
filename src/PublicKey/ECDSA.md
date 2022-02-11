# Elliptic curve DSA public-key signature scheme

```haskell
module PublicKey.ECDSA
  (
    WECDSAParams(..)
  , WECDSAKeyPair, WECDSAPublicKey
  , genWECDSAKeyPair
  , wecdsaSign, wecdsaVerifySignature
  ) where

import Bytes ( HasBytes(..), convBytes )
import Bytes.Integral ()
import Hash ( sha1Hash )
import Modulo ( mkMod, modulo )
import EllipticCurve ( WECParameters(..), WECPoint(..), mkWEC, modWEC )
import PublicKey ( KeyPair(..), PublicKey(..) )
import Random ( randomResidue )

import Data.Semigroup ( stimes )

import Math.NumberTheory.Primes.Testing ( isPrime )

import qualified Control.Monad.Random as R
```

Our three DSA parameters are moved into elliptic curve territory:
we have a `WECParameters` instead of a prime modulus,
and our base g is a `WECPoint` of order q.

```haskell
data WECDSAParams = WECDSAParams
  { wecdsaParams :: WECParameters
  , wecdsaG :: WECPoint
  , wecdsaQ :: Integer }
  deriving (Eq,Ord,Show)
```

The private key is still an integer,
while the public key is an EC point, the generator raised to that power.

```haskell
type WECDSAKeyPair = KeyPair WECDSAParams WECPoint Integer
type WECDSAPublicKey = PublicKey WECDSAParams WECPoint
```

Our operations are straightforward translations of the modular DSA.

```haskell
genWECDSAKeyPair :: R.MonadRandom m => WECDSAParams -> m WECDSAKeyPair
genWECDSAKeyPair params = do
  let WECDSAParams{ wecdsaParams = ps, wecdsaG = g, wecdsaQ = q} = params
  x <- randomResidue q
  let y = (x `stimes` mkWEC g) `modWEC` ps
  pure KeyPair{ kpParameters = params, kpPrivate = x, kpPublic = y }
```

In signing, the r value used is the x coordinate of the computed EC point.

```haskell
wecdsaSign :: (HasBytes text, R.MonadRandom m)
           => WECDSAKeyPair -> text -> m (Integer,Integer)
wecdsaSign kp text = genRS
 where
  hash = convBytes (sha1Hash text)
  WECDSAParams{ wecdsaParams = ps, wecdsaQ = q, wecdsaG = g } = kpParameters kp

  genRS = do
    k <- randomResidue q
    let r = wecX ((k `stimes` mkWEC g) `modWEC` ps) `rem` q
        s = (mkMod (hash + kpPrivate kp * r) / mkMod k) `modulo` q
    if r == 0 || s == 0
      then genRS
      else pure (r,s)
```

```haskell
wecdsaVerifySignature :: HasBytes text
                      => WECDSAPublicKey -> text -> (Integer,Integer) -> Bool
wecdsaVerifySignature pk text (r,s)
  | r <= 0 || s <= 0 || q <= r || q <= s = False
  | otherwise =
    let hash = convBytes (sha1Hash text)
        w = (recip $ mkMod s) `modulo` q
        u1 = (hash * w) `rem` q
        u2 = (r * w) `mod` q
        v = wecX $ ((u1 `stimes` mkWEC g) <>
                    (u2 `stimes` mkWEC (pkKey pk))) `modWEC` ps
    in  r `mod` q == v `mod` q
 where
  WECDSAParams{ wecdsaParams = ps
              , wecdsaQ = q, wecdsaG = g } = pkParameters pk
```
