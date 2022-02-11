# RSA public-key cryptosystem

```haskell
module PublicKey.RSA
  (
    RSAParams, RSAKeyPair, RSAPublicKey
  , genRSAKeyPair
  , cryptRSA
  , rsaBlockSize, rsaBlockSize'
  , rsaSign, rsaVerifySignature
  , sha1Identifier
  ) where

import Bytes ( HasBytes(..), Bytes, convBytes )
import Bytes.Hex ( mkHex )
import Bytes.Integral ()
import Hash ( sha1Hash )
import Util ( cdiv )
import Modulo ( mkMod, modulo, (^%) )
import Padding.PKCS1 ( padPKCS1', validatePKCS1 )
import Random ( randomPrimeR )
import PublicKey ( KeyPair(..), PublicKey(..), publicPart )

import Control.Monad ( guard )
import Data.Maybe ( fromJust, fromMaybe )

import qualified Data.ByteString as B
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

We need an idea of "block size" for an RSA message.
This is just the number of bytes taken up by the modulus:

```haskell
rsaBlockSize' :: Integer -> Int
rsaBlockSize' = numBytes
```

The "normal" version of this extracts n from the public key.

```haskell
rsaBlockSize :: RSAPublicKey -> Int
rsaBlockSize PublicKey{ pkKey = (_,n) } = rsaBlockSize' n
```


## RSA message signatures

Signing a message with RSA involves several steps.

```haskell
rsaSign :: HasBytes text => RSAKeyPair -> text -> Maybe Integer
rsaSign kp message =
```

First, we hash the message.
A number of hashes could be used, but here we'll use SHA-1.
We affix an identifier specifying which hash function was used.

```haskell
  let hashedMsg = sha1Identifier <> toBytes (sha1Hash message)
```

We then pad with PKCS1 padding (type 1).

```haskell
      block = padPKCS1' (rsaBlockSize $ publicPart kp) hashedMsg
```

Finally, we encrypt the block with the private key.

```haskell
  in  cryptRSA (kpPrivate kp) <$> block
```

The [ASN.1 identifier for SHA1](https://tools.ietf.org/html/rfc8017#page-47) is

```haskell
sha1Identifier :: Bytes
sha1Identifier = toBytes . fromJust . mkHex $ "3021300906052b0e03021a05000414"
```

To validate a signed message, we strip off the padding then verify the hash.
Note that we want to avoid the problem exploited in Challenge 42:
we have to ensure that the digest runs all the way to the end of the block!

```haskell
rsaVerifySignature :: (HasBytes text, HasBytes sig)
                   => RSAPublicKey -> text -> sig -> Bool
rsaVerifySignature pk message sig = fromMaybe False $ do
```

We decrypt with the public key and strip off the padding.

```haskell
  let block = cryptRSA (pkKey pk) sig
      blockSize = rsaBlockSize pk
  stripped <- validatePKCS1 blockSize block
```

The stripped message should be the SHA-1 identifier
followed by the hash digest.
We verify both parts.

```haskell
  let (iden,digest) = B.splitAt (numBytes sha1Identifier) stripped
  guard $ iden == sha1Identifier
  pure $ digest == toBytes (sha1Hash message)
```
