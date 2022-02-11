# Tools for handling and comparing discrete distributions

In several of the Challenges,
we want to compare discrete distributions.
This module contains the `Distribution` type,
as well as statistical tools for comparing distributions.

```haskell
module Distribution
  (
    Distribution
  , countBytes
  , logLikelihood
  , chi2Test
  ) where

import Bytes ( HasBytes(..) )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
import qualified Data.Vector as V

import qualified Statistics.Types as S
import qualified Statistics.Test.ChiSquared as S
```

A distribution, in the sense of this module,
is the number of observations for each possible symbol.
If the possible symbols are `Byte`s,
then we can make this more efficient by storing the observation counts
in a `Vector` of length 256.

```haskell
type Distribution = V.Vector Int
```

We can count the occurences of each byte in a `Bytes`
to create an observed distribution.

```haskell
countBytes :: HasBytes text => text -> Distribution
countBytes text =
  let bytes = [ (fromIntegral b, 1) | b <- B.unpack (toBytes text) ]
      zeroDistribution = V.replicate 256 0
  in  V.accum (+) zeroDistribution bytes
```

### Log-likelihood

We want to determine, given a number of possible decryptions,
which one best fits a given distribution.
This is a statistical
[*goodness-of-fit*](https://en.wikipedia.org/wiki/Goodness_of_fit)
problem.
For small sample sizes, a good statistical test for this is *likelihood*.
The likelihood corresponding to a sample distribution
is the probability that it is generated under the target distribution.
The probability that any given observation $x$
is drawn from the distribution $P$ is just $P(x)$;
the probability that an entire sequence $x[i]$ is drawn from $P$
is then $\prod _ i P(x[i])$.
If byte $j$ occurs $O[j]$ times in the sequence,
then this is just $\prod _ j P(j) ^ {O[j]}$.
If we don't care about the order of the sequence,
we find that the likelihood is just $N! \prod _ j (P(j) ^ {O[j]}) / O[j]!$.
This value can get very small, so we compute instead its logarithm;
since the logarithm function is monotonic,
the maximum will be in the same place. 

```haskell
logLikelihood :: Distribution -> Distribution -> Double
logLikelihood dist obs =
```

We first turn our integer counts into `Double`s
so we can use `log` and the like.

```haskell
  let dist' = fmap fromIntegral dist
      obs' = fmap fromIntegral obs
```

The first `Distribution` is our target,
and we need the actual probabilities from it;
we therefore normalize the values so they sum to 1.

```haskell
      sumDist = sum dist'
      prob = fmap (/ sumDist) dist'
```

We then combine the probabilities with the observed values
using the function `logProb` and sum the values to get our log likelihood.

```haskell
  in  sum $ V.zipWith logProb prob obs'
```

The `logProb` function simply computes
$\log ((P ^ O) / O!) = O * \log(P) - \log(O!)$.
If the number of observations is zero,
we default the `logProb` to zero to avoid `NaN`s in the computaion.

```haskell
 where
  logProb _ 0 = 0
  logProb p o = o * log p - logFac o
  logFac k = sum $ map log [2 .. k]
```

### The chi-squared test

For larger sample sizes, we can use the
[chi-squared test](https://en.wikipedia.org/wiki/Chi-squared_test)
instead.

```haskell
chi2Test :: Distribution-> Distribution -> Double
chi2Test dist obs =
```

We must rescale the distribution to have the same sum as the observations:

```haskell
  let dmult = fromIntegral (sum obs) / fromIntegral (sum dist)
      dist' = fmap ((* dmult) . fromIntegral) dist
```

We can then run the chi-squared test from the
[statistics](https://hackage.haskell.org/package/statistics) package.
If there is not enough data, it will return `Nothing`;
we assume that there is enough data.

(The first argument to `chi2test` is the number of extra degrees of freedom;
since all of our observations are independent, it's zero.)

```haskell
      chi2 = fromJust $ S.chi2test 0 $ V.zip obs dist'
```

We then return the p-value of the statistic,
which is (roughly) the likelihood the observation
is drawn from the distribution.

```haskell
  in  S.pValue $ S.testSignificance chi2
```
