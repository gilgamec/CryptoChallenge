# Tools for timing attacks

This module contains a few types and utilities
for performing timing attacks.

```haskell
{-# LANGUAGE NumDecimals #-}

module Timing
  (
    TimingResponse(..), TimingOracle
  , getHiresTime
  , insecure_compare
  ) where

import Bytes ( Bytes )

import Control.Concurrent ( threadDelay )
import Data.Time.Clock.System ( SystemTime(..), getSystemTime )

import qualified Data.ByteString as B
```

A timing attack breaks a cryptosystem by exploiting the different
times taken to process different requests.

A `TimingOracle` is a function that takes a request
and either succeeds or fails, returning the time taken to fail.
It runs in some monad, probably `IO`.

```haskell
type TimingOracle m request = request -> m TimingResponse

data TimingResponse = RequestFailed Int | RequestSucceeded
  deriving (Eq,Ord,Show)
```

Timing attacks depend on measuring the time taken to high accuracy.
The function `getHiresTime` returns the current system time in nanoseconds.
(The actual clock resolution is somewhat lower.)

```haskell
getHiresTime :: IO Int
getHiresTime = do
  st <- getSystemTime
  let seconds = fromIntegral (systemSeconds st)
      nanoseconds = fromIntegral (systemNanoseconds st)
  pure $ 1e9*seconds + nanoseconds
```

One source of timing leaks is in comparisons:
normally when comparing two sequences for equality,
we can return as soon as a difference is found,
but this means that an attacker can tell how early the sequences differ
because it will take longer to compare nearly identical sequences.
This effect is very small, certainly in a compiled language
and sampled over a network connection,
so we amplify it in the `insecure_compare` function.
This function compares two byte strings for equality,
returning as soon as a difference is found,
and inserting an artificial delay between each comparison.

We unpack the input byte strings into lists of bytes
and recurse over the lists.

```haskell
insecure_compare :: Int -> Bytes -> Bytes -> IO Bool
insecure_compare delay a b = testEq (B.unpack a) (B.unpack b)
 where
```

If one or both lists are empty,
we know immediately whether they are equal or not
and can return.

```haskell
  testEq [] [] = pure True
  testEq _  [] = pure False
  testEq [] _  = pure False
```

Otherwise, we test the next byte in both lists.
If they differ, we can immediately return `False`.

```haskell
  testEq (a:as) (b:bs)
    | a /= b = pure False
```

If they are equal then we must continue comparing the rest of the bytes.
Before that, however, we delay by the given number of microseconds.

```haskell
    | a == b = threadDelay delay >> testEq as bs
```
