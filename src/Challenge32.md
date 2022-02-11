# Solution to Challenge 32

```haskell
module Challenge32
  (
    timingAttack
  ) where

import Bytes ( Bytes, Byte )
import Bytes.Hex ( showHex )
import Hash ( SHA1MAC )
import Timing ( TimingOracle, TimingResponse(..) )

import Challenge31 ( fakeSHA1MAC )

import Data.Ord ( comparing )

import qualified Control.Monad.Trans.Except as E

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Mutable as MV

import qualified Control.Monad.Random as R
import qualified Data.ByteString as B
import qualified Crypto.Hash as H

import qualified Statistics.Types as S
import qualified Statistics.Test.MannWhitneyU as S

import Debug.Trace ( trace )
```

The test decoding for the function 'timingAttack' stops working consistently
for a comparison delay of about 30ms.
To handle lower delay times,
we're going to have to average the time over several requests,
which is going to require statistics (and
[statistics](https://hackage.haskell.org/package/statistics)).

The failure of the simple timing attack makes sense
when you consider how long the queries can take.
Consider the function `getNextByte` from Challenge 31.
We time our request for every possible byte
and want to pick the one which does one extra comparison.
However, the query time is not only dependent on the delay time;
there's also some natural variation, because of unpredictable differences
from memory paging to thread scheduling to network latency.

In a run where I timed the performance of webserver queries
without any artificial comparison delay,
I saw sample standard deviations of about 400 microseconds per query;
this is perhaps possible to handle with the simple method.
However, some query times can get much, much higher.
After a hundred thousand tests, the longest query I observed took nearly 40ms!
Over the more than 5000 queries necessary
to find all of the bytes in a SHA-1 hash,
it's nearly inevitable that we'll run into a random whale of a query time
that screws up our measurements unless the artificial delay is comparably high.

Indeed, this is what we observe in actual runs with smaller delay times;
most bytes are guessed correctly, then one is guessed wrong and,
since we have no mechanism to recover from this,
the entire search fails.

A more sophisicated approach is needed.
Instead of just selecting the query that took the longest time,
we will make multiple queries for each byte
and look at the likelihood that a given set of queries
performed more comparisons than any of the other queries.
That is, for each query byte we want to compare
the times taken for queries of that byte,
versus the times taken for all other queries,
and determine how likely it is that the average of the former
is greater than that of the latter.
If the query time were normally distributed, we could use
[Student's t-test](https://en.wikipedia.org/wiki/Student%27s_t-test);
however, observing the actual distribution of times shows
that it is far from normal!

Another method we can use, which does not assume anything about
the distributions, is the
[Mann-Whitney U-test](https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test).
The Mann-Whitney test is pretty simple in outline.
All of the samples, from all query bytes, are sorted and ranked;
we then take the sum of the ranks for each byte.
If a given byte takes consistently longer to query,
we expect that its ranks should be higher.
It turns out that the rank sum is normally distributed,
so we can quantify this likelihood.

The [statistics](https://hackage.haskell.org/package/statistics) package
includes a Mann-Whitney test,
but running it repeatedly on almost the same data
results in much writing and rewriting of `Vector`s;
in particular, it has to re-sort the samples every call.
With the structure of our data, however,
it's possible to maintain the samples as a sorted `Vector`
and compute all of the rank sums in a single pass.

The `sortAndMerge` function takes our sorted `Vector` of measurements
and a new set of timings, another (unsorted) `Vector`.
It then merges the two into our new timings `Vector`.

```haskell
sortAndMerge :: V.Vector (Byte,Double) -> V.Vector (Byte,Double)
             -> V.Vector (Byte,Double)
sortAndMerge sorted unsorted =
```

We first sort the unsorted vector, using `sortBy` from
[vector-algorithms](https://hackage.haskell.org/package/vector-algorithms).

```haskell
  let sorted2 = V.modify (V.sortBy $ comparing snd) unsorted
```

The length of the final vector is the total length of the two input vectors.

```haskell
      len = V.length sorted + V.length unsorted
```

Its elements are created by merging the two sorted vectors.

```haskell
  in  V.fromListN len $ merge sorted sorted2
```

The `merge` function is pretty simple;
it just recurses down the elements of the two vectors,
always taking the one that is least.

```haskell
 where
  merge v1 v2
    | V.null v1 = V.toList v2
    | V.null v2 = V.toList v1
    | otherwise =
      let x1 = V.unsafeHead v1
          x2 = V.unsafeHead v2
      in  case compare (snd x1) (snd x2) of
            LT -> x1 : merge (V.unsafeTail v1) v2
            _  -> x2 : merge v1 (V.unsafeTail v2)
```

With all of our time samples in a sorted `Vector`,
we can compute the rank sums for all the query bytes in a single pass.
This can be done pretty simply with the `accum` function.

```haskell
rankSums :: V.Vector (Byte,a) -> V.Vector Int
rankSums timings = V.unsafeAccum (+) (V.replicate 256 0)
                      [ (fromIntegral b, i)
                      | ((b,_),i) <- zip (V.toList timings) [0..] ]
```

With these rank sums, we can find which bytes have sample times
significantly longer than the rest.
The function `significantU` takes the observation vector
as well as the desired significance, or P-value, to use as the cutoff point.

```haskell
significantU :: S.PValue Double -> V.Vector (Byte,Double) -> [Byte]
significantU pval timings =
```

We assume that all query bytes have the same number of samples;
then the number of samples per byte is the total number of samples
divided by 256. For each byte, we consider the number of samples for that byte
with the number of samples for all of the other bytes.

```haskell
  let numSamples = V.length timings
      numPerByte = numSamples `div` 256
      numOtherBytes = numSamples - numPerByte
```

We normalize the rank sum by subtracting off the lowest possible rank sum,
effectively shifting the distribution to start from zero.
This gives us the Mann-Whitney U measure.

```haskell
      lowestRankSum = (numPerByte * (numPerByte - 1)) `div` 2
      uScores = fmap (fromIntegral . subtract lowestRankSum) (rankSums timings)
```

We can now use the `statistics` package's builtin test for significance
(which is conveniently separated from the U-value computation).

```haskell
      totalU = fromIntegral $ numOtherBytes * numPerByte
      uTest = S.mannWhitneyUSignificant S.AGreater (numPerByte,numOtherBytes)
      isSignificant b =
        let (u1,u2) = (uScores V.! fromIntegral b, totalU - u1)
        in  uTest pval (u1,u2) == Just S.Significant
```

We return only the bytes whose sample times are significantly greater than 
the rest.

```haskell
  in  filter isSignificant [0..255]
```

The byte timings themselves are generated by the function `getByteTimings`.
It can be run on any subset of the query bytes,
and normally returns a `Vector` of a pair of (query byte, time in ms);
however, we use an `Except` transformer to allow it to return the full hash
if one of the queries succeeds.

```haskell
getByteTimings :: R.MonadRandom m
               => TimingOracle m (SHA1MAC a) -> a -> Bytes -> [Byte]
               -> E.ExceptT Bytes m (V.Vector (Byte,Double))
getByteTimings oracle x known candidates = do
```

First, we randomly permute the input vector (see below).

```haskell
  bytes <- R.lift $ randomPermuteVector (V.fromList candidates)
```

Next, we call the oracle on all of the possible queries
and collect the results.

```haskell
  res <- R.lift $ mapM (oracle . fakeSHA1MAC x . B.snoc known) bytes
```

We traverse over the results to look for query successes
and convert from nanoseconds to milliseconds.

```haskell
  let f (b,req) = case req of
                    RequestSucceeded -> E.throwE (known `B.snoc` b)
                    RequestFailed ns -> pure (b, fromIntegral ns / 1e6)
  V.mapM f $ V.zip bytes res
```

One of the biggest biases I observed in the simple version of this Challenge
is that earlier queries in a set take significantly longer than the average.
In order to counter this, we will do the queries in a random order.
The function `randomPermuteVector` performs a random permutation of
an input `Vector`.

```haskell
randomPermuteVector :: R.MonadRandom m => V.Vector a -> m (V.Vector a)
randomPermuteVector vec = do
```

The [Fisher-Yates shuffle](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle)
swaps the elements of the vector, from the end to the beginning,
with a randomly-chosen element up to their position in the vector.
This essentially requires the generation of `n-1` pairs of swap partners.

```haskell
  let len = V.length vec
      ixs = [len-1,len-2..1]
  jxs <- R.forM ixs $ \n -> R.getRandomR (0,n)
```

We then modify the vector by performing all of the swaps.

```haskell
  pure $ V.modify (\mv -> sequence_ (zipWith (MV.unsafeSwap mv) ixs jxs)) vec
```

The new `getNextByte` function finds the next byte of the signature,
using the Mann-Whitney U-test to test for significance.
It may return either the complete hash (if one of the requests succeeds)
or just the next byte.

```haskell
getNextByte :: R.MonadRandom m
            => TimingOracle m (SHA1MAC a) -> a -> Bytes
            -> E.ExceptT Bytes m Byte
getNextByte oracle x known =
```

We'd like a confidence interval of something like 0.05 or 0.01.
Remember, though, that we're actually looking at 256 different tests;
thus, we expect about 0.05 * 256, or about 13 of them
to succeed at a 0.05 confidence interval!
We can use the (very simple)
[Bonferroni correction](https://en.wikipedia.org/wiki/Bonferroni_correction),
which just divides the desired confidence interval by the number of tests.

```haskell
  let excludeThresh = S.mkPValue (0.01 / 256)
```

`nextSamples` generates a new set of samples,
merges them into the current collection,
then tests for significance.

```haskell
      nextSamples oldSamples = do
        newSamples <- getByteTimings oracle x known [0..255]
        let samples = oldSamples `sortAndMerge` newSamples
            significant = significantU excludeThresh samples
```

If there is exactly one significant byte, then we return it;
otherwise, we recurse to gather more samples.

```haskell
        case significant of
          [b] -> pure b
          _   -> nextSamples samples
```

We start off the process with no samples.

```haskell
  in  nextSamples V.empty
```

Our updated timing attack uses the new `getNextByte`.
It works pretty much the same as the simple version,
allowing for a shortcut when one of the hashes is validated.

```haskell
timingAttack :: R.MonadRandom m
             => TimingOracle m (SHA1MAC a) -> a -> m (SHA1MAC a)
timingAttack oracle x =
  let addByte known = do
        known' <- B.snoc known <$> getNextByte oracle x known
        pure $ (trace $ "decoded HMAC: " <> showHex known') known'
      digestSize = H.hashDigestSize H.SHA1
      op = foldl (>>=) (pure B.empty) $ replicate digestSize addByte
  in  fakeSHA1MAC x . either id id <$> E.runExceptT op
```
