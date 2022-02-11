# Generation of random values

This module contains functions which let us generate
random values.

```haskell
module Random
  (
    randomBytes, randomBytesR
  , randomResidue
  , randomPrimeR
  ) where

import Bytes ( Bytes )

import qualified Data.ByteString as B
import qualified Control.Monad.Random as R

import Math.NumberTheory.Primes.Testing ( isPrime )
```

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
