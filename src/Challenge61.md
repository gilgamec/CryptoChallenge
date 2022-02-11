# Solution to Challenge 61

```haskell
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}

module Challenge61
  (
    sigDuplicateWEC
  , sigDuplicateRSA
  ) where

import Bytes ( HasBytes(..), convBytes )
import PublicKey ( KeyPair(..), PublicKey(..) )
import PublicKey.ECDSA ( WECDSAParams(..), WECDSAPublicKey )
import PublicKey.RSA ( RSAKeyPair, RSAPublicKey
                     , rsaBlockSize, rsaBlockSize', sha1Identifier )
import Hash ( sha1Hash )
import Padding.PKCS1 ( padPKCS1' )
import Modulo ( mkMod, getVal, modulo, withMod, (^%), mkMultMod, getMultMod )
import Random ( randomResidue )
import EllipticCurve ( mkWEC, modWEC )
import GroupOps ( pohligHellman )
import Util ( cdiv, xgcd )

import Control.Monad ( guard )
import Data.List ( (\\), group, sort )
import Data.Maybe ( listToMaybe )
import Data.Semigroup ( stimes )
import Data.Word ( Word )

import Math.NumberTheory.Primes.Testing ( isPrime )
import Math.NumberTheory.Primes ( nextPrime, precPrime, unPrime )

import qualified Control.Monad.Random as R
import qualified Data.Vector as V
```

## Signature duplication in WECDSA

This is pretty simple; we go through the verification process
until we get to the WEC point

    v = (u1 * g) + (u2 * pubkey).

```haskell
sigDuplicateWEC :: (R.MonadRandom m, HasBytes message)
                => WECDSAPublicKey -> message -> (Integer,Integer)
                -> m WECDSAPublicKey
sigDuplicateWEC pk message (r,s) =
  let ps@WECDSAParams{ wecdsaParams = params
                     , wecdsaQ = q, wecdsaG = g } = pkParameters pk
      hash = convBytes (sha1Hash message)
      w = (recip $ mkMod s) `modulo` q
      u1 = (hash * w) `rem` q
      u2 = (r * w) `mod` q
      v = (u1 `stimes` mkWEC g) <> (u2 `stimes` mkWEC (pkKey pk))
```

In a valid signature, this point is r.
But we can detatch g from the rest of the point:

    v = (u1 * g) + (u2 * privkey * g)
      = (u1 + u2*privkey)*g.

So given a random private key d, all we need is to find g' such that

    v = (u1 + u2*d)*g'     i.e.
    g'= (u1 + u2*d)^-1 * v

```haskell
  in  do
    d' <- randomResidue q
    let tInv = (recip $ mkMod (u1 + u2 * d')) `modulo` q
        g' = tInv `stimes` v
    pure PublicKey{ pkKey = (d' `stimes` g') `modWEC` params
                  , pkParameters = ps{ wecdsaG = g' `modWEC` params } }
```

## Signature duplication in RSA

Now we do something similar in RSA.
We verify a signature s of a message m by encrypting it with the public key,
stripping the padding, and checking the hash; i.e., making sure that

    s^e = pad(hash(m)) mod n

We want to find a public key (e',n') such that

    s^e' = pad(hash(m)) mod n'.

Basically, this will come down to (carefully) choosing n' = p' * q',
such that finding the discrete log of pad(hash(m)) with base s is easy.

We're going to have to find discrete logarithms in many subgroups
of both p' and q'; it's going to be easier if they're both very smooth,
i.e. if (p'-1) and (q'-1) have lots of small prime factors.
The function `smoothPrime` creates a prime within the given bounds
whose odd factors are all elements of the given list.
We return not only the prime, but also its factorization.

```haskell
smoothPrime :: R.MonadRandom m
            => (Integer,Integer) -> [Integer] -> m (Integer,[(Integer,Word)])
smoothPrime (lb,ub) primes =
```

For ease of indexing, we put the factors into a `Vector`.

```haskell
  let vec = V.fromList primes
      pidx = (vec V.!)
```

We create our prime number by testing the primality of a bunch of
different numbers made up from the numbers in the list.
We're going to do this by creating a (potentially infinite)
random sequence of these numbers, and doing a standard subsequence search
to find a subsequence whose product is in the given bounds and is prime.

The subsequence search is textbook. We carry two accumulators:
the current sequence of factors, and their product;
we recurse through a list of the remaining factors.

```haskell
      sss :: [Integer] -> Integer -> [Integer] -> (Integer,[Integer])
```

If the sequence is empty, we just pull a single factor, multiply it
into the current value, and recurse.

```haskell
      sss []     n (r:rs) = sss [r] (n*r) rs
```

Otherwise, there are number of steps we can take.
If the value is too high, we remove a factor from the subsequence.

```haskell
      sss (x:xs) n (r:rs)
        | n > ub = sss xs (n `div` x) (r:rs)
```

If the value is too low, we move the next factor into the subsequence.

```haskell
        | n < lb = sss (x:xs ++ [r]) (n * r) rs
```

If neither of the previous guards triggers,
the value is in range, so if n+1 is prime, we return it.

```haskell
        | isPrime (n+1) = (n+1, x:xs)
```

Otherwise, we increase the subsequence exactly as in the n<lb case.

```haskell
        | otherwise = sss (x:xs ++ [r]) (n * r) rs
```

We generate the random sequence by generating an infinite list
of indices from the list.

```haskell
  in  do
    ps <- map pidx <$> R.getRandomRs (0, length primes - 1)
```

We use the subsequence search to find our prime.
We have to start with a single factor of 2
(which will not be in our input list).

```haskell
    let (p,fs) = sss [] 2 ps
```

We turn the sequence of factors into a list of factors and exponents
by sorting, grouping, and converting:

```haskell
    let fs' = map (\gs -> (head gs, fromIntegral (length gs))) $
              group $ sort fs
```

Finally, we return the prime and factor-exponent list.

```haskell
    pure (p,fs')
```

In addition to the primes being smooth,
we must make sure that neither s nor pad(hash(m))
are contained in any nontrivial subgroup of Zp.

```haskell
factoringPrime :: R.MonadRandom m
               => (Integer,Integer) -> [Integer] -> [Integer]
               -> m (Integer, [(Integer,Word)])
factoringPrime bounds primes xs = genPrime
 where
```

We try to generate a factoring prime
by generating a smooth prime and testing it against all factor numbers.

```haskell
  genPrime = do
    (p,fes) <- smoothPrime bounds primes
    if and [ isPrimitiveRoot p f x | (f,_) <- fes, x <- xs ]
      then pure (p,fes)
      else genPrime
```

The check is to make sure that
the factor numbers' powers x^((p-1) / f) are not 1 mod p,
for all prime factors f of p-1.

```haskell
  isPrimitiveRoot p f x = (mkMod x ^% ((p-1) `div` f)) `modulo` p /= 1
```

We create a new n' by finding two factoring primes p, q whose product:

1. has the same blocksize as n (so pad(hash(m)) is the same)

2. is greater than s (so s still fits inside it)

```haskell
factoringPQ :: R.MonadRandom m
            => Integer -> Integer -> Integer -> m (Integer,Integer)
factoringPQ n s hash = do
```

The block size of n in bits is

```haskell
  let nBits = 8 * rsaBlockSize' n
```

n' then has to be between 2^(nBits - 8) and 2^nBits;
it must also be greater than s.

```haskell
  let loBound = max s (2 ^ (nBits - 8))
      hiBound = 2 ^ nBits
```

The smooth primes are made from primes less than 2^16.

```haskell
  let hiPrime = 2^16
      smallPrimes = map unPrime [ nextPrime 2 .. precPrime hiPrime ]
```

Our factors for the factoring prime are s and pad(hash(m)):

```haskell
  let xs = [s,hash]
```

We will choose a p within two bits (on either side) of
the square root of our desired size.

```haskell
  let pBits = nBits `quot` 2
  (p,pfes) <- factoringPrime (2^(pBits-2), 2^(pBits+3)) smallPrimes xs
```

q-1 cannot have any factors (other than 2) in common with p-1.
We remove them from the list of primes before generating q.

```haskell
  let smallPrimes' = smallPrimes \\ map fst pfes
```

Since loBound < pq < hiBound, we have tight bounds on q.

```haskell
  (q,_) <- factoringPrime (loBound `cdiv` p, hiBound `div` p) smallPrimes' xs
```

We finally return p and q.

```haskell
  pure (p,q)
```

Finally we can write the RSA signature duplicator.

```haskell
sigDuplicateRSA :: (R.MonadRandom m, HasBytes message)
                => RSAPublicKey -> message -> Integer -> m RSAKeyPair
sigDuplicateRSA pk message s = genKP
 where
  PublicKey{ pkKey = (_,n) } = pk
```

We hash and pad the message.

```haskell
  blockSize = rsaBlockSize pk
  hashed = sha1Identifier <> toBytes (sha1Hash message)
  hash :: Integer
  Just hash = convBytes <$> padPKCS1' blockSize hashed
```

We want to generate factoring primes p and q
to create a new key based on n' = p*q.
Not every pair of factoring primes will work, however,
so we have to check them.

The function `goodKP` returns a good keypair made from p and q,
if one exists.

```haskell
  goodKP :: (Integer,Integer) -> Maybe RSAKeyPair
  goodKP (p,q) = do
```

We compute the discrete log of the hash modulo p and q
using the Pohlig-Hellman algorithm.

```haskell
    let cat = fromIntegral . getVal . getMultMod
    plog <- withMod p (pohligHellman cat (mkMultMod s) (p-1)) (mkMultMod hash)
    qlog <- withMod q (pohligHellman cat (mkMultMod s) (q-1)) (mkMultMod hash)
```

To get the correct discrete log mod p * q,
we would like to use something like the Chinese remainder theorem.
Since (p-1) and (q-1) aren't relatively prime (they are both even),
the pure Chinese remainder theorem doesn't apply;
however, we know that if plog = qlog (mod gcd (p-1) (q-1)),
then we still have a unique solution mod lcm (p-1) (q-1);
we can then add on factors of the lcm to find
all possible solutions mod (p-1)*(q-1).

```haskell
    let n' = p * q
        (g,(np,nq)) = xgcd (p-1) (q-1)
        l = lcm (p-1) (q-1)
```

We require that plog = qlog (mod g).

```haskell
    guard $ (plog - qlog) `mod` g == 0
```

The Chinese remainder theorem then gives us one solution, mod l:

```haskell
    let x = ((plog * nq * (q-1) + qlog * np * (p-1)) `div` g) `mod` l
```

The other solutions are then of the form x + k*l, for k < g.
We grab one which works as a public key,
i.e. successfully forges the signature.

```haskell
    e <- listToMaybe [ e | k <- [0..g-1], let e = x + k * l
                         , ((mkMod s ^% e) `modulo` n') == hash ]
```

We then create a private key counterpart
and return the key pair.

```haskell
    let d = recip (mkMod e) `modulo` ((p-1)*(q-1))
    pure KeyPair{ kpParameters = ()
                , kpPublic = (e,n')
                , kpPrivate = (d,n') }
```

Now we just have to generate factoring primes p and q
until we get a pair that works.

```haskell
  genKP = do
    pq <- factoringPQ n s hash
    case goodKP pq of
      Nothing -> genKP
      Just kp -> pure kp
```
