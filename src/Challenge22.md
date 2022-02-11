# Solution to Challenge 22

The Challenge is titled "Crack an MT19937 seed", but it really means
"Why you shouldn't use the current time as a seed".

```haskell
module Challenge22
  (
    crackMTSeed
  ) where

import MersenneTwister ( mtExtract, mtSeed )

import Data.Word ( Word32 )

import Data.Time.Clock.POSIX ( POSIXTime )
```

The routine the Challenge asks for gets a random value
by seeding the MT generator with the current time,
then returning its first 32-bit output.

Cracking this is really simple, since you know the current time
(which is soon after the time used to seed the generator).
We just try all of the previous times until we find a seed
that gives us the right value.

```haskell
crackMTSeed :: Word32 -> POSIXTime -> Word32
crackMTSeed val now =
  let nowSeed = floor now
      seeds = [nowSeed, nowSeed-1 ..]
      firstVal = fst . mtExtract . mtSeed
  in  head $ filter ((==val) . firstVal) seeds
```
