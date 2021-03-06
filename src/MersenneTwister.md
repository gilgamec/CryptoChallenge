# Mersenne Twister

This module implements the Mersenne Twister MT19937 PRNG, based on
[the pseudocode on Wikipedia](https://en.wikipedia.org/wiki/Mersenne_Twister#Pseudocode).

```haskell
module MersenneTwister
  (
    MTState(..)
  , mtSeed, mtExtract, mtExtractMany
  , cloneMT
  ) where

import Control.Monad ( forM_ )
import Data.Bits ( Bits(..) )
import Data.Bifunctor ( first )
import Data.List ( foldl', scanl', tails )
import Data.Word ( Word32 )

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
```

The Mersenne Twister has a *state*,
which is simply a block of 32-bit values,
each of which will be *tempered* to create an output value.
Once these values are exhausted, it *twists* the state
to generate the next block of values.
It thus has to also keep track of how many values
it has generated so far for this block.

```haskell
data MTState = MT
  { mtCycle :: {-# UNPACK #-} !Int
  , mtState :: !(V.Vector Word32) }
  deriving (Eq,Ord,Show)
```

The state vector will consist of 624 32-bit words.
These, along with other magic numbers,
are required by the algorithm.

```haskell
mtN = 624
mtW = 32
```

A new state is created by *seeding* the random number generator,
given an initial seed value.

```haskell
mtSeed :: Word32 -> MTState
mtSeed seed =
```

The seed becomes the first element of the state vector,
and the rest is generated by the recurrence

    x_i = f * (x_i-1 XOR (x_i-1 >> (w-2))) + i.

```haskell
  let fmult x i = mtF * (x `xor` (x `shiftR` (mtW - 2))) + i
      state = V.scanl fmult seed $ V.fromListN (mtN - 1) [1..]
```

The initial cycle number is set to `N`, the length of the state vector,
signalling that the state will have to be twisted before a number is produced.

```haskell
  in  MT{ mtCycle = mtN, mtState = state }
```

`f`, the seed generation parameter, is another magic number.

```haskell
mtF = 1812433253
```

`mtExtract` gets the next value.
It returns both the value generated and the new `MTState`.

```haskell
mtExtract :: MTState -> (Word32,MTState)
```

If the cycle is `N`, then we've exhausted the state vector
and have to twist it before extracting a value.

```haskell
mtExtract mt
  | mtCycle mt == mtN = mtExtract (mtTwist mt)
```

Otherwise, we get the next value and temper it,
while advancing the counter.

```haskell
  | otherwise = ( mtTemper $ mtState mt V.! mtCycle mt
                , mt{ mtCycle = succ (mtCycle mt) } )
```

We can extract a list of many generated values using `mtExtractMany`.

```haskell
mtExtractMany :: Int -> MTState -> ([Word32],MTState)
mtExtractMany 0 st = ([],st)
mtExtractMany n st =
  let (x,st') = mtExtract st
  in  first (x:) $ mtExtractMany (n-1) st'
```

The tempering procedure consists of four shift-and-xor operations
using magic numbers.

```haskell
mtTemper :: Word32 -> Word32
mtTemper y0 =
  let y1 = y0 `xor` ((y0 `shiftR` mtU) .&. mtD)
      y2 = y1 `xor` ((y1 `shiftL` mtS) .&. mtB)
      y3 = y2 `xor` ((y2 `shiftL` mtT) .&. mtC)
  in  y3 `xor`  (y3 `shiftR` mtL)

mtU = 11
mtD = 0xFFFFFFFF

mtS = 7
mtB = 0x9D2C5680

mtT = 15
mtC = 0xEFC60000

mtL = 18
```

Finally, the twisting procedure completely rewrites the state vector.
The cycle number is reset to zero.

```haskell
mtTwist :: MTState -> MTState
mtTwist MT{ mtState = state } = MT 0 (twist state)
 where
```

The twist operation uses both old and new values of the state vector,
so we implement it with a mutable vector operation.
We perform a loop over each element in the vector,
from beginning to end.

```haskell
  twist :: V.Vector Word32 -> V.Vector Word32
  twist = V.modify $ \vec -> forM_ [0..mtN - 1] $ \i -> do
    let next1 = (i+1) `mod` mtN
        nextM = (i+mtM) `mod` mtN
    mti  <- MV.read vec i
    mti1 <- MV.read vec next1
    let x   = (mti .&. upperMask) + (mti1 .&. lowerMask)
        xA' = x `shiftR` 1
        xA  = if testBit x 0 then xA' `xor` mtA else xA'
    mtim <- MV.read vec nextM
    MV.write vec i $ mtim `xor` xA
```

Here we have more magic numbers.

```haskell
mtR = 31
mtM = 397
mtA = 0x9908B0DF

lowerMask, upperMask :: Word32
lowerMask = (1 `shiftL` mtR) - 1
upperMask = complement lowerMask
```

## Inverting the Mersenne Twister

Given enough successive outputs of a Mersenne Twister,
we can clone its state and thus predict its future behaviour.

First, we have to go from output value to internal state
by inverting the tempering operation.
For reference, this operation is

```haskell ignore
mtTemper y0 =
  let y1 = y0 `xor` ((y0 `shiftR` mtU) .&. mtD)
      y2 = y1 `xor` ((y1 `shiftL` mtS) .&. mtB)
      y3 = y2 `xor` ((y2 `shiftL` mtT) .&. mtC)
  in  y3 `xor`  (y3 `shiftR` mtL)
```

There are two different kinds of operations here:

1. y2 and y3 are computed by XORing their predecessors
   against left-shifted versions of themselves,
   ANDed by a magic number;
2. y1 and y4 (the return value) are computed by XORing their predecessors
   against right-shifted versions of themselves, without the AND
   (mtD is 0xffffffff, so the AND against it is a no-op).

We therefore just need to `unShiftL` and `unShiftR`.

```haskell
mtUntemper :: Word32 -> Word32
mtUntemper = unShiftR mtU .
             unShiftL mtS mtB .
             unShiftL mtT mtC .
             unShiftR mtL
```

`unShiftR` is undoing a XOR against a right-shifted version, i.e.

    y = x ^ (x >> r)  so  x = y ^ (x >> r).

We substitute the x from the left into the right and get

    x = y ^ ((y ^ (x >> r)) >> r) = y ^ (y >> r) ^ (x >> 2r)

and repeat to see that

    x = y ^ (y >> r) ^ (y >> 2r) ^ (y >> 3r) ^ ...

Of course, this can't go on forever because eventually the shift
becomes more than 32 bits and all remaining terms are zero.

The function just constructs the terms and folds `xor` over them.

```haskell
unShiftR :: Int -> Word32 -> Word32
unShiftR r y = foldl' xor 0 $ map (shiftR y) [0,r..32]
```

`unShiftL` is slightly more difficult because we have to deal with
the mask we're ANDing against the term.

In this case we have `y = x ^ (m & (x << l))` so

    x = y ^ (m & (x << l))
      = y ^ (m & ((y ^ (m & (x << l))) << l))
      = y ^ (m & ((y << l) ^ ((m << l) & (x << 2l)))).

The AND distributes and we have

    x = y ^ (m & (y << l)) ^ (m & (m << l) & (x << 2l))
      = y ^ (m & (y << l)) ^ (m & (m << l) & (y << 2l)) ^
            (m & (m << l) & (m << 2l) & (y << 3l)) ^ ...

Again, eventually we shift more than 32 bits so `y << kl = 0`.

```haskell
unShiftL :: Int -> Word32 -> Word32 -> Word32
unShiftL l m y =
```

The y-part of each term is just a successively longer shift of the original y.

```haskell
  let ys = map (shiftL y) [0,l..32]
```

The mask part, on the other hand, is the product of a bunch of
more-shifted copies of m.
We compute it with a scan whose first element will be `0xffffffff`,
i.e. an empty mask.

```haskell
      masks = scanl' (.&.) 0xffffffff $ map (shiftL m) [0,l..32]
```

We AND together the two parts of each term and XOR it all up.

```haskell
  in  foldl' xor 0 $ zipWith (.&.) ys masks
```

Using `mtUntemper` we can clone an MT19937 PRNG
given a stream of generated values.
It might be thought that, since each value is taken from the state block,
that we need to untemper one entire block's worth of values
to create the exact same state.
However, it turns out that *any* sequence of `N` values,
untempered and stuck into a block,
will create a state that reproduces exactly the value stream,
even if none of the actual states are reproduced themselves.
This makes cloning very simple,
since we don't have to wait for the start of a block.

```haskell
cloneMT :: [Word32] -> MTState
cloneMT vals = MT{ mtCycle = 0
                 , mtState = V.fromListN mtN (map mtUntemper vals) }
```
