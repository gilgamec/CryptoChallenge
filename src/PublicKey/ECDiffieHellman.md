# Diffie-Hellman public-key cryptosystem over elliptic curves

```haskell
module PublicKey.ECDiffieHellman
  (
    WECDHParams(..), WECDHKeyPair, WECDHPublicKey
  , genWECDHKeyPair, wecdhSharedSecret

  , MECDHParams(..), MECDHKeyPair, MECDHPublicKey
  , genMECDHKeyPair, mecdhSharedSecret
  ) where

import EllipticCurve ( WECParameters(..), WECPoint, mkWEC, modWEC
                     , MECParameters(..), MECPoint, mecLadder )
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

## Montgomery form

The Montgomery form is pretty much identical to the Weierstrass form.

```haskell
data MECDHParams = MECDHParams
  { mecdhParams :: MECParameters
  , mecdhBase :: MECPoint
  , mecdhOrder :: Integer }
  deriving (Eq, Ord, Show)

type MECDHKeyPair = KeyPair MECDHParams MECPoint Integer
type MECDHPublicKey = PublicKey MECDHParams MECPoint
```

We use the Montgomery ladder for exponentiation instead of the Monoid instance.

```haskell
genMECDHKeyPair :: R.MonadRandom m => MECDHParams -> m MECDHKeyPair
genMECDHKeyPair params@(MECDHParams p g q) = do
  private <- randomResidue q
  pure $ KeyPair { kpParameters = params
                 , kpPrivate = private
                 , kpPublic = mecLadder p private g }
```

 The shared secret is computed in the same way.

```haskell
mecdhSharedSecret :: MECDHKeyPair -> MECDHPublicKey -> MECPoint
mecdhSharedSecret kp pk =
  let KeyPair{ kpParameters = MECDHParams p _ _, kpPrivate = n } = kp
      PublicKey{ pkKey = gm } = pk
  in  mecLadder p n gm
```
