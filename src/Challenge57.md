# Solution to Challenge57

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}

module Challenge57
  (
    genDHKeyPair'
  , bobEncoder
  , getPrivateKey
  ) where

import Bytes ( HasBytes(..) )
import Hash ( MAC(..), SHA1MAC, mkHMACSHA1, validateHMACSHA1 )
import PublicKey ( KeyPair(..), PublicKey(..) )
import PublicKey.DiffieHellman ( DHParams(..), DHKeyPair, DHPublicKey
                               , dhSharedSecret )
import Modulo ( mkMod, getVal, modulo, withMod2, (^%), MultMod(..), mkMultMod )
import GroupOps ( pohligHellmanOracle )
import Random ( randomResidue )

import qualified Control.Monad.Random as R
```

We generate the limited DH key pair by acknowledging that the private key
is really only meaningful modulo the order q of the generator g.

```haskell
genDHKeyPair' :: R.MonadRandom m => DHParams -> Integer -> m DHKeyPair
genDHKeyPair' params@(DHParams p g) q = do
  private <- randomResidue q
  pure $ KeyPair{ kpParameters = params
                , kpPrivate = private
                , kpPublic = (mkMod g ^% private) `modulo` p }
```

We're going to find Bob's private key by getting him to authenticate a message
using our (broken) public key.

```haskell
bobEncoder :: HasBytes message
           => DHKeyPair -> message -> DHPublicKey -> SHA1MAC message
bobEncoder keypair msg pubkey = mkHMACSHA1 (dhSharedSecret keypair pubkey) msg
```

Since the sample DH prime has lots of small prime factors,
we can use the small-factor Pohlig-Hellman solver to get our root.

```haskell
getPrivateKey :: (HasBytes message, R.MonadRandom m)
              => (DHPublicKey -> SHA1MAC message) -> DHPublicKey
              -> Integer -> m Integer
getPrivateKey oracle pubKey q = do
  let PublicKey{ pkParameters=DHParams{dhModulus=p} } = pubKey
```

The solver will run over the multiplicative group modulo p.
We need an operation to generate a random residue.

```haskell
      mkRan = mkMultMod <$> randomResidue p
```

The solver also needs an oracle to indicate when it's found the right exponent.
We will know this because we'll be able to generate and validate a broken MAC.

```haskell
      oracle' (MultMod m) =
        let mac = oracle pubKey{ pkKey = getVal m }
        in  \k -> validateHMACSHA1 (getVal (m ^% k)) mac
```

With that set up, we call the Pohlig-Hellman solver.

```haskell
  (prv,_) <- withMod2 p (\rng -> pohligHellmanOracle rng oracle' (p-1)) mkRan
  pure (prv `rem` q)
```
