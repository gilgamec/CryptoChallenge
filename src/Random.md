# Generation of random values

This module contains functions which let us generate
random values.

```haskell
module Random
  (
    randomBytes, randomBytesR
  , randomResidue
  ) where

import Bytes ( Bytes )

import qualified Data.ByteString as B
import qualified Control.Monad.Random as R
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
