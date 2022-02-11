# Diffie-Hellman public-key cryptosystem

```haskell
module PublicKey.DiffieHellman
  (
    DHParams(..), DHKeyPair, DHPublicKey
  , genDHKeyPair, dhSharedSecret
  , prime1536
  ) where

import Bytes ( convBytes )
import Bytes.Hex ( mkHex )
import Bytes.Integral ()
import Modulo ( mkMod, modulo, (^%) )
import Random ( randomResidue )
import PublicKey ( KeyPair(..), PublicKey(..) )

import Data.Maybe ( fromJust )

import qualified Control.Monad.Random as R
```

The Diffie-Hellman cryptosystem's parameters are
a prime modulus `p` and the generator `g`.

```haskell
data DHParams = DHParams{ dhModulus :: Integer, dhGenerator :: Integer }
  deriving (Eq, Ord, Show)
```

Both public and private keys are residues of the finite field of size `p`;
we represent them as `Integer`s.

```haskell
type DHKeyPair = KeyPair DHParams Integer Integer
type DHPublicKey = PublicKey DHParams Integer
```

We generate a key pair by creating a random exponent `k` (our private key).

```haskell
genDHKeyPair :: R.MonadRandom m => DHParams -> m DHKeyPair
genDHKeyPair params@DHParams{ dhModulus = p, dhGenerator = g } = do
  private <- randomResidue p
```

The public key is then `g ^ k mod p`.

```haskell
  pure $ KeyPair{ kpParameters = params
                , kpPrivate = private
                , kpPublic = (mkMod g ^% private) `modulo` p }
```

The Diffie-Hellman shared secret computed by B is

    (A's public key ^ B's private key) mod p;

since A's public key is `(g ^ A's private key)`,
the shared secret computed by B is is

    g ^ (A's private key * B's private key),

which is clearly the same as the secret computed by A.

```haskell
dhSharedSecret :: DHKeyPair -> DHPublicKey -> Integer
dhSharedSecret KeyPair{ kpParameters = params, kpPrivate = n }
               PublicKey{ pkKey = gm } =
  (mkMod gm ^% n) `modulo` dhModulus params
```

We also have the 1536-bit safe prime suggested in
[RFC3526](https://www.ietf.org/rfc/rfc3526.txt) for key exchange.

```haskell
prime1536 :: Integer
prime1536 = convBytes . fromJust . mkHex . concat $
  [ "ffffffffffffffffc90fdaa22168c234c4c6628b80dc1cd129024"
  , "e088a67cc74020bbea63b139b22514a08798e3404ddef9519b3cd"
  , "3a431b302b0a6df25f14374fe1356d6d51c245e485b576625e7ec"
  , "6f44c42e9a637ed6b0bff5cb6f406b7edee386bfb5a899fa5ae9f"
  , "24117c4b1fe649286651ece45b3dc2007cb8a163bf0598da48361"
  , "c55d39a69163fa8fd24cf5f83655d23dca3ad961c62f356208552"
  , "bb9ed529077096966d670c354e4abc9804f1746c08ca237327fff"
  , "fffffffffffff"
  ]
```
