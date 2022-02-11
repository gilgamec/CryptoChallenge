# Generation of random values

This module contains functions which let us generate
random values.

```haskell
module Random
  (
    randomBytes, randomBytesR
  , randomResidue
  , randomPrimeR
  , randomWECPoint
  ) where

import Bytes ( Bytes )
import EllipticCurve ( WECParameters(..), WECPoint, mkWECPoint )
import Math ( modSqrt )

import qualified Data.ByteString as B
import qualified Control.Monad.Random as R

import Math.NumberTheory.Primes.Testing ( isPrime )
```

## Random bytes

`randomBytes` produces a sequence of the given number of random `Byte`s.

```haskell
randomBytes :: R.MonadRandom m => Int -> m Bytes
randomBytes len = B.pack <$> R.replicateM len R.getRandom
```

`randomBytesR` produces a random number of random `Byte`s.
Its argument is the lower and upper bound on the number of `Byte`s to produce.

```haskell
randomBytesR :: R.MonadRandom m => (Int,Int) -> m Bytes
randomBytesR bounds = R.getRandomR bounds >>= randomBytes
```

## Random integers

`randomResidue` produces a random element of the
finite field of the given size.
For usability as a key, we exclude 0, 1, and p-1.

```haskell
randomResidue :: R.MonadRandom m => Integer -> m Integer
randomResidue p = R.getRandomR (2,p-2)
```

`randomPrimeR` produces a prime number within the given bounds.
It generates a random integer, then tests its primality
using `isPrime` from the
[arithmoi](https://hackage.haskell.org/package/arithmoi) package.

```haskell
randomPrimeR :: R.MonadRandom m => (Integer,Integer) -> m Integer
randomPrimeR bounds = do
  n <- R.getRandomR bounds
  if isPrime n
    then pure n
    else randomPrimeR bounds
```

## Random elliptic curve points

`randomWECPoint` produces a random point on the elliptic curve
with the given Weierstrass-form parameters.

```haskell
randomWECPoint :: R.MonadRandom m => WECParameters -> m WECPoint
randomWECPoint params@WECParameters{wecA=a, wecB=b, wecP=p} = do
```

`x` can potentially take any value in the prime field.

```haskell
  x <- randomResidue p
```

We can then compute y^2 from the elliptic curve equation.

```haskell
  let x2 = (x*x) `rem` p
      y2 = ((x2 + a)*x + b) `rem` p
```

Unfortunately, this value might not have a square root.
If not, then we have to pick another x and try again.

```haskell
  case modSqrt p y2 of
    Nothing -> randomWECPoint params
```

If there is a square root y, the point could be either (x,y) or (x,-y);
we randomly determine which and return.

```haskell
    Just y  -> do
      neg <- R.getRandom
      pure $ mkWECPoint params x $ if neg then (-y) else y
```
