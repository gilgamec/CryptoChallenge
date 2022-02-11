# RSA public-key cryptosystem

```haskell
module PublicKey.RSA
  (
    RSAParams, RSAKeyPair, RSAPublicKey
  , genRSAKeyPair
  , cryptRSA
  ) where

import Bytes ( HasBytes(..), convBytes )
import Bytes.Integral ()
import Util ( cdiv )
import Modulo ( mkMod, modulo, (^%) )
import Random ( randomPrimeR )
import PublicKey ( KeyPair(..), PublicKey(..) )

import qualified Control.Monad.Random as R
```

RSA has no parameters.

```haskell
type RSAParams = ()
```

Both public and private keys are pairs of `Integer`s:
the exponent and the modulus.

```haskell
type RSAKeyPair = KeyPair RSAParams (Integer,Integer) (Integer,Integer)
type RSAPublicKey = PublicKey RSAParams (Integer,Integer)
```

The RSA key pair generation function
takes as parameter the desired number of bits
of the modulus `n`.

```haskell
genRSAKeyPair :: R.MonadRandom m => Int -> m RSAKeyPair
genRSAKeyPair bits = do
```

We generate a key pair of the specified number of bits
by creating two random primes p and q of about half as many bits.

```haskell
  let bp = bits `div` 2
  p <- randomPrimeR (2^(bp-2),2^(bp+1))
  let qmin = (2^(bits-1)) `cdiv` p
      qmax = (2^bits) `div` p
  q <- randomPrimeR (qmin,qmax)
```

The math is done modulo n = p * q,
which has totient t = (p-1) * (q-1).

```haskell
  let n = p * q
      t = (p-1) * (q-1)
```

Our public key e is 3.
If d * e = 1 mod t, then m^(ed) = m mod n.
Because only we know p and q (and thus t)
only we can compute d; it is our private key.

```haskell
  let e = 3
      d = (recip $ mkMod e) `modulo` t
```

It is possible (if p and q are not safe primes) that e will divide t;
in this case, there's no inverse d.
We check this and try the generation again if so.

```haskell
  case gcd e t of
    1 -> pure $ KeyPair{ kpParameters = ()
                       , kpPublic = (e,n)
                       , kpPrivate = (d,n) }
    _ -> genRSAKeyPair bits
```

We encrypt with RSA by taking the message to the power of the exponent, mod n.
This operation works with both public and private keys,
so we have a single function to handle it.

```haskell
cryptRSA :: HasBytes text => (Integer,Integer) -> text -> Integer
cryptRSA (e,n) m = (fromInteger (convBytes m) ^% e) `modulo` n
```
