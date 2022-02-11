# Diffie-Hellman public-key cryptosystem over elliptic curves

```haskell
module PublicKey.ECDiffieHellman
  (
    WECDHParams(..), WECDHKeyPair, WECDHPublicKey
  , genWECDHKeyPair, wecdhSharedSecret
  ) where

import EllipticCurve ( WECParameters(..), WECPoint, mkWEC, modWEC )
import Random ( randomResidue )
import PublicKey ( KeyPair(..), PublicKey(..) )

import Data.Semigroup ( stimes )

import qualified Control.Monad.Random as R
```

## Weierstrass form

The parameters of DH are the elliptic curve parameters,
the base g, and its order q.

```haskell
data WECDHParams = WECDHParams
  { wecdhParams :: WECParameters
  , wecdhBase :: WECPoint
  , wecdhOrder :: Integer }
  deriving (Eq, Ord, Show)
```

The private key is an integer, and the public key is the EC point g^x.

```haskell
type WECDHKeyPair = KeyPair WECDHParams WECPoint Integer
type WECDHPublicKey = PublicKey WECDHParams WECPoint
```

Generating a key pair is essentially identical to regular DH.

```haskell
genWECDHKeyPair :: R.MonadRandom m => WECDHParams -> m WECDHKeyPair
genWECDHKeyPair params@(WECDHParams p g q) = do
  private <- randomResidue q
  pure $ KeyPair { kpParameters = params
                 , kpPrivate = private
                 , kpPublic = (private `stimes` mkWEC g) `modWEC` p }
```

 The shared secret is also computed in a parallel way.

```haskell
wecdhSharedSecret :: WECDHKeyPair -> WECDHPublicKey -> WECPoint
wecdhSharedSecret kp pk =
  let KeyPair{ kpParameters = WECDHParams p _ _, kpPrivate = n } = kp
      PublicKey{ pkKey = gm } = pk
  in  (n `stimes` mkWEC gm) `modWEC` p
```
