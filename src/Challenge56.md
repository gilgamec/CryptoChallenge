# Solution to Challenge 56

```haskell
module Challenge56
  (
    rc4Oracle
  , rc4Tables, freqTables
  , findAllBytes
  ) where

import Bytes ( HasBytes(..), Bytes, Byte, xor )
import Random ( randomBytes )
import Distribution ( chi2Test )

import Control.Monad ( forM, forM_, replicateM )
import Control.Monad.Primitive ( PrimMonad )
import Data.Bifunctor ( second )
import Data.Function ( on )
import Data.List ( sort, groupBy )
import Data.Maybe ( mapMaybe )

import qualified Control.Monad.Random as R
import qualified Crypto.Cipher.RC4 as RC4
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
```

We need an RC4 oracle which generates a random key and
encrypts the plaintext, prefixed by the given text.

```haskell
rc4Oracle :: (HasBytes secret, HasBytes request, R.MonadRandom m)
          => secret -> request -> m Bytes
rc4Oracle secret request = do
  key <- randomBytes 16
  pure $ snd $ RC4.combine (RC4.initialize key) str
 where
  str = toBytes request <> toBytes secret
```

We know that the RC4 keystream has biases, especially
at the second position z2, and at every sixteenth position z16, z32, ...
This means that if we call the encryption oracle many times,
we expect that the distribution of bytes at each position
is not uniform, but exhibits these same biases.
For instance, the key at byte 2 is twice as likely to be zero
as any other value; if we evaluate an RC4 of the unknown secret many times,
it is overwhelmingly likely that the most common byte found in the second
position is the actual plaintext byte p2 (as 0 XOR p2 = p2).
The later biases aren't so obvious, so we'll have to do something more clever.

But ... isn't this just the same problem as waaaay back in Challenge 3,
where we determined the "Englishness" of a text by comparing its distribution
of characters against actual English text?
We can in fact use exactly the same method here: we sample a whole
bunch of RC4 instances to find the distribution of the keystream
for the bytes in question. Then we start calling the oracle and accumulate
the byte distributions at each position, and compare the various distributions
XORed with each candidate byte against the known biased distribution.

The function `rc4Tables` accumulates the distribution of
keystream bytes in given positions.
The first argument is the oracle,
the second number of ciphers to generate,
and the third is the list of byte positions to sample
(for this Challenge, positions 16 and 32).

```haskell
rc4Tables :: (Traversable t, R.MonadRandom m, PrimMonad m)
          => m Bytes -> Int -> t Int -> m (t (V.Vector Int))
rc4Tables oracle num ps = do
```

We store the byte counts in a mutable `Vector`;
we create one for each byte position we have to sample.
(We subtract one to zero-index the sample positions.)

```haskell
  counts <- traverse (\p -> MV.replicate 256 0 >>= \mv -> pure (p-1,mv)) ps
```

We count the value for a single ciphertext at a position
by incrementing the element of the count vector
corresponding to the byte in the given position.

```haskell
  let incVec bs p mv = MV.modify mv (+1) (fromIntegral $ B.index bs p)
```

We then generate `num` ciphers
and count every desired position in them.

```haskell
  replicateM num $ do
    bs <- oracle
    forM_ counts $ uncurry (incVec bs)
```

Finally, we freeze the count vectors into regular `Vector`s and return.

```haskell
  traverse (V.freeze . snd) counts
```

From a series of observations, we can build 256 different distributions,
one for XORing against each possible `Byte`.

```haskell
allXORs :: V.Vector Int -> V.Vector (V.Vector Int)
allXORs dist = V.generate 256 $ \i ->
               V.generate 256 $ \j ->
               dist V.! (i `xor` j)
```

Now we can search for bytes within an RC4-encrypted secret.
We are given the oracle and a list of the bytes we are to look for,
along with their XORed distributions.

```haskell
findBytes :: (R.MonadRandom m, PrimMonad m)
          => m Bytes -> Int -> [(Int, V.Vector (V.Vector Int))] -> m [Byte]
findBytes oracle batchSize pds = readBatch >>= mainLoop
 where
```

Rather than recompute the statistics after every cipher generated,
we process an entire batch of samples at at time,
adding the counts to the running totals.

```haskell
  readBatch = rc4Tables oracle batchSize (map fst pds)
  nextBatch obs = zipWith (V.zipWith (+)) obs <$> readBatch
```

We want to keep acquiring cipher samples until we have determined
the correct byte to within some statistical significance.
(What we're actually measuring is that every other distribution
is *not* a match, to within this significance.)

```haskell
  significance = 0.05
```

For each byte position, we have a distribution for each possible byte value.
We want to check the observed byte values against each of these distributions.
The differences between distributions are going to be far less
than we had for English text in the earlier Challenge,
so we're going to need more samples,
and thus we can use the chi-squared test to test goodness-of-fit.

```haskell
  compareObs (_,vdss) obs = [ (fromIntegral bval, chi2Test dist obs)
                            | (bval,dist) <- zip [0..] (V.toList vdss) ]
```

We only want to proceed if all but one byte value
differs significantly from the observations.

```haskell
  sigTest bs = case filter ((> significance) . snd) bs of
                 [(b,s)] -> Just b
                 _       -> Nothing
```

In the main loop, we compute these likelihoods for all byte positions
and only proceed if *all* of them have been found to our satisfaction.

```haskell
  mainLoop obs = case traverse sigTest $ zipWith compareObs pds obs of
                   Just bs -> pure bs
```

Otherwise, we grab another batch of ciphers and continue.

```haskell
                   Nothing -> nextBatch obs >>= mainLoop
```

To find every byte, we want to run `findBytes` for every given shift value
to find the bytes that will be under the windows at that shift value.
It takes as argument the oracle (without the shift) and the list of windows,
along with their corresponding precomputed byte distributions.

```haskell
findAllBytes :: (R.MonadRandom m, PrimMonad m)
             => (Bytes -> m Bytes) -> [(Int,V.Vector Int)] -> m Bytes
findAllBytes oracle pvs = do
```

We're going to need the extended (per-byte) versions of the frequency tables,
so we compute them first.

```haskell
  let pds = map (second allXORs) pvs
```

We compute the cipher with no additional bytes to find out
how long the message is; i.e. how many bytes we have to find.

```haskell
  len <- numBytes <$> oracle B.empty
```

We find the minimum shift needed to get each byte under a window
and group them by this number.
(This is mostly list wrangling; in the end, `byShift` relates the
shift size with the list of byte positions placed under a window by it.)

```haskell
  let minShift i = minimum $ mapMaybe (\(p,_) -> if i <= p
                                                 then Just (p-i)
                                                 else Nothing) pvs
      shifts = [ (minShift i, i) | i <- [1..len] ]
      grpShifts = groupBy ((==) `on` fst) $ sort shifts
      byShift = [ (shift, bs) | gs <- grpShifts, let (shift:_,bs) = unzip gs ]
```

For each shift amount, we call `findBytes` to get the bytes
at each mapped position.

```haskell
  bbs <- forM byShift $ \(shift,bs) -> do
```

The oracle is padded by `shift` bytes.

```haskell
    let oracle' = oracle (B.replicate shift 0)
```

We pull the distributions for the byte positions we're looking for.

```haskell
    let dists = filter ((`elem` bs) . subtract shift . fst) pds
```

We then run `findBytes` with a reasonable batch size.

```haskell
    findBytes oracle' 1000000 dists
```
  
Finally, we re-sort the bytes and return them.
(This requires undoing the previous list wrangling.)

```haskell
  pure $ B.pack $ map snd $ sort $ concat $ zipWith zip (map snd byShift) bbs
```

### Precomputed byte distributions

Running `rc4Tables` over the weekend gives us `freqTables`,
a precomputed table of byte frequencies drawn from about 42 billion ciphers.

```haskell
freqTables :: [(Int,V.Vector Int)]
freqTables = zip [16,32] $ map (V.fromListN 256)
  [[164961897,164076599,164048936,164081990,164097209,164083675,164097697
   ,164104188,164099426,164067780,164083175,164090954,164086887,164099400
   ,164098343,164012492,164926420,163970209,164003455,163960685,163987036
   ,163953262,163982713,163993315,163987743,163979907,163978358,163985905
   ,163986696,163980401,163977630,163946836,163949316,163984675,163985228
   ,163983269,163971819,164004433,163971310,163998735,163971301,164021823
   ,164002463,163977374,163986094,164025529,164022638,163987229,163996693
   ,163996711,164012586,164010466,164005548,164030931,164022453,164001866
   ,164028707,164015827,164004501,164016215,164012099,164003278,164021226
   ,164011251,164009299,164014903,164015142,164028261,164028094,164037999
   ,164014204,164023536,164000516,164021564,164035148,164005856,164031495
   ,164020698,164038333,164044966,164017667,164021159,164054349,164034919
   ,164059360,164048465,164043725,164050623,164027319,164037739,164075027
   ,164040307,164040543,164039933,164038566,164051157,164064282,164052162
   ,164041855,164057725,164062994,164064806,164036670,164056982,164054439
   ,164068227,164063049,164073360,164073917,164075680,164062577,164065420
   ,164071872,164090923,164057542,164094303,164078191,164058611,164072208
   ,164069606,164093305,164069372,164112526,164111871,164082507,164114129
   ,164086607,164092230,164116285,164109324,164100928,164110567,164093948
   ,164113024,164099607,164096235,164100749,164115730,164107524,164129207
   ,164108034,164116293,164107423,164116753,164136519,164121054,164100194
   ,164141276,164149255,164115850,164150829,164155404,164125797,164116166
   ,164131684,164131407,164119387,164150941,164138836,164106576,164151307
   ,164148750,164125028,164146474,164172296,164168094,164149195,164151813
   ,164182121,164142309,164170165,164161341,164151892,164187605,164173893
   ,164174529,164199565,164153892,164200682,164184459,164198862,164174102
   ,164201443,164186084,164186886,164206244,164186566,164219625,164207065
   ,164210050,164200764,164195653,164221217,164228841,164208441,164233927
   ,164244601,164211224,164244745,164235081,164232162,164216030,164239895
   ,164264057,164243033,164258610,164256834,164243857,164228416,164271976
   ,164241945,164259287,164256296,164239133,164267618,164279232,164269526
   ,164275444,164284146,164282481,164263824,164260525,164314839,164276403
   ,164266235,164323946,164272175,164301491,164286512,164327817,164309498
   ,164312113,164329131,164337764,164309385,164326702,164311846,164333694
   ,164322227,164340639,170002408,164422625,164399859,164404107,164384150
   ,164392633,164395741,164403187,164421881,164421483,164433463,164419406
   ,164423165,164441542,164423926,164438410]
  ,[164875598,164063948,164078338,164087210,164110833,164087795,164094034
   ,164096140,164085707,164088971,164089013,164069392,164102016,164093449
   ,164088434,164084701,164166147,164101968,164090237,164091561,164060201
   ,164085091,164102021,164087849,164126497,164097517,164098518,164098631
   ,164111760,164116206,164102322,164048162,164778470,163993549,164009278
   ,163984624,163980883,164005878,164009698,163982279,163968058,163992356
   ,164005830,163993607,164001367,164012172,164009543,163999942,164018831
   ,163999332,164026200,164015966,164015341,164003702,164002633,164007608
   ,164021915,164001294,164021790,164002466,164036803,164002556,164025990
   ,163993716,164025574,164025530,164019640,164019124,164065564,164017772
   ,164029110,164034017,164041344,164022081,164045398,164013823,164062568
   ,164028207,164070786,164024880,164057995,164022091,164052104,164040726
   ,164069102,164033514,164050625,164034523,164056950,164035220,164071560
   ,164054458,164080890,164041201,164062040,164049124,164056176,164056924
   ,164070490,164064402,164063131,164044324,164092265,164050302,164064532
   ,164056170,164068030,164060492,164094459,164036317,164074492,164070515
   ,164085176,164081498,164114629,164084123,164087932,164076526,164076542
   ,164060410,164113254,164092798,164102646,164089079,164084311,164087865
   ,164089643,164083861,164098803,164079169,164109272,164086789,164107157
   ,164097062,164113187,164103728,164116385,164106658,164127188,164116246
   ,164136810,164106122,164102708,164100175,164120188,164112408,164135383
   ,164130751,164119209,164104830,164163348,164114946,164159896,164131861
   ,164165138,164137040,164125882,164131670,164149552,164167715,164169752
   ,164127852,164156344,164151954,164147591,164142751,164175329,164154083
   ,164174038,164146520,164168689,164161556,164196277,164162095,164173912
   ,164173882,164186740,164172135,164192667,164170790,164189760,164167557
   ,164204976,164195506,164208714,164166580,164199574,164186261,164193129
   ,164178007,164217158,164221151,164219277,164203259,164207367,164199747
   ,164234290,164216188,164243200,164227304,164231949,164234196,164235842
   ,164230361,164216650,164202953,164229677,164225752,164265474,164236513
   ,164269343,164227974,164251361,164256362,164248133,164245524,164267706
   ,164271032,164287363,164253531,164297735,164282167,164277689,164294360
   ,168089439,164278927,164320100,164286846,164349098,164300563,164323229
   ,164324457,164322731,164334397,164365871,164297617,164369667,164353631
   ,164376959,164338735,164342489,164375616,164398910,164378660,164372693
   ,164363551,164384141,164365371,164422823,164375112,164403932,164382833
   ,164420475,164383909,164423750,164377784]
  ]
```
