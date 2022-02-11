# Solution to Challenge 65

```haskell
{-# LANGUAGE MultiWayIf #-}

module Challenge65
  (
    findH
  ) where

import Bytes ( HasBytes(..), Bytes, convBytes, chunksOf, xorb, splitEnd )
import Bytes.Integral ( bigEndian )
import BitMatrix ( BitVector, BitMatrix, rowBasis, kernelBasis, solutionSpace )
import GCM ( GF )
import Hash ( MAC(..) )
import Util ( cdiv, interleave )

import Challenge64 ( GCMMACTrunc(..), multMat, tMatrix, aMatrix )
import qualified Challenge64

import Data.Bits ( Bits(..), FiniteBits(..) )
import Data.List ( scanl' )
import Data.Maybe ( listToMaybe, mapMaybe )

import Math.NumberTheory.Logarithms ( integerLog2 )

import qualified Numeric.LinearAlgebra as M
import qualified Data.ByteString as B
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
```

RECALL from the previous Challenge:
Given ciphertext blocks ci, we want to create blocks c'i
that hash to the same (truncated) MAC. The MAC is of the form

    s + z*h + c_1*h^2 + c_2*h^3 + ...

(where s is the nonce and z is the size descriptor).
Taking h to the power of any power of 2 is a linear operation,
so we'll leave most cis the same and change only c1, c3, c7, etc.

The difference in this Challenge is that we will also change z,
the size descriptor, if the ciphertext does not fill out an entire block.
The size block (with empty AD) is `(64 zeros | big-endian rep. of ct length)`.
Say the final ciphertext block (c1) is only two bytes (16 bits) long.
If we change the size block to `(64 zeros | big-endian rep. of len + 112)`,
we have indicated that c1 takes up the entire 128-bit block.
This gives us an entire block to play with.
More importantly, since the zero-block is multiplied by h to a power of 2,
this is also a linear operation, and we want to find bitflips d such that

    0 = sum (d_j * h^(2^j)) + (z-z0) * h
      = sum (d_j * sqr^j * h) + mul(dz) * h
      = (sum ( mul(d_j) * sqr^j ) + mul(dz)) * h
	  = (Ad + mul(dz)) * h.

We want to find forgeries so we can find new rows perpendicular to h,
and eventually pinpoint the value of h.

In Challenge 64, we created a matrix T whose columns consisted of
the first (n-1) rows of Ad, for every possible single bitflip d;
we had to leave one row nonzero so we would generate forgeries outside
the currently known space for h.
However, if we modify z, then the equation with T becomes `T * d = Tz`,
where Tz is the column of the first few rows of the matrix mul(dz).
If we make T square, i.e. zero out the first n rows of Ad,
then we have a single, nonzero solution for a bitflip d which
ensures that the first n bits of the MAC are unchanged.

How is this useful?
In the previous Challenge, we found a space of ds which keep some bits unchanged;
then we could search them for a specific d which keeps all bits unchanged,
and must try ~2^(r-n) of them. Another way to proceed, however,
is to apply a single bitflip which leaves some bits unchanged,
then change *the MAC itself* until we find the correct one;
there are again 2^(r-n) possibilities.
We will then have `Ad * h = b = (original mac XOR forged mac)`
and can solve this equation to reduce our H estimate.

We don't want to go all the way to zeroing out all but one row of A;
when we do that, in half the cases we get an equation r*h=1,
which tells us almost nothing about h!
Instead, we'll stop when we have few enough bits
that brute-forcing a forgery is easy, but we still have enough rows that
we should be able to get a couple from each forgery.
We choose this bit threshold to be 4; then at the end of the computation,
we will find a forgery once in every 16 attempts,
and improve our estimate by up to 3 bits.
(Note that after row reduction, at most one row will have dh value 1,
i.e. we get at least three of four rows orthogonal to h,
no matter what the values in dh.)
 
```haskell
chooseK :: Int -> Int -> BitMatrix -> Int
chooseK r n x = min (Challenge64.chooseK r n x) (r - 4)
```

`lenAMatrix` produces the A matrix created by a change to the length block.
There is no power, so this is simply the multiplication matrix.

```haskell
lenAMatrix :: Bytes -> BitMatrix
lenAMatrix = multMat . fromBytes
```

`bitFlipSolutions` returns all nonzero solutions for `Tk*d=Tz`.
It takes the number of rows to zero k and the number of power blocks n,
as well as the alteration to the length block dz.

```haskell
bitFlipSolutions :: Int -> Int -> Bytes -> BitMatrix -> [Bytes]
bitFlipSolutions k n dz x =
  let tk = tMatrix k n x
```

Tz is computed as a column vector:

```haskell
      tz = M.subVector 0 (k * M.cols x) $ M.flatten $ lenAMatrix dz <> x
```

We find all solutions to the equation `Tk * d = Tz`.

```haskell
  in  case solutionSpace tk tz of
```

If there are no solutions, we return an empty list.

```haskell
        Nothing -> []
```

Otherwise, we can generate all of the possible solutions,
filter out a zero solution if we get one, and return the list.

```haskell
        Just (sol,basis) -> map toBytes $
                            filter (not . VS.all (==0)) $
                            genSolutions sol basis
```

The list of solutions is generated not by creating a bunch of
coordinate vectors and combining as appropriate,
but instead by generating a Gray code telling us which
basis element to add to an accumulating value.

```haskell
genSolutions :: BitVector -> [BitVector] -> [BitVector]
genSolutions sol basis =
  let len = length basis
      basisV = V.fromListN len basis
```

There are potentially lots of entries in the solution space;
we only want to compute some of them and choose to take up to 2^32.

```haskell
      numEntries = 2 ^ (min len 32)
```

The Gray code is created by computing the number of trailing zeroes
in a standard binary enumeration.

```haskell
      grayCode = map countTrailingZeros [1::Int ..]
```

We use the Gray code to index into the vector of basis elements
and get our sequence of XORs to apply.

```haskell
      xors = map (basisV V.!) $ take (numEntries - 1) grayCode
```

Finally, we perform a scan over the list of XORs,
starting with the original solution, to produce our list of solutions.

```haskell
  in  scanl' (+) sol xors
```

`hashDeltasFor` performs a brute-force search for
the hashes which validate a given MAC,
given the number of bits which can be held constant;
it returns the deltas between them and the real hash.

```haskell
hashDeltasFor :: Int -> (GCMMACTrunc nonce ad -> Bool) -> GCMMACTrunc nonce ad
              -> [Bytes]
hashDeltasFor k oracle mac =
  let hash = macHash mac
```

The total number of bits in the truncated GCM hash is

```haskell
      r = 8 * numBytes hash
```

The first k bits are held constant.
We can alter the rest according to some index
by XORing against a mask of the bottom (r-k) bits.

```haskell
      bottomMask = 2^(r-k) - 1 :: Integer
      dH i = bigEndian (numBytes hash) (i .&. bottomMask)
```

We then return all of the hashes that validate.

```haskell
  in  [ delta | i <- [1..bottomMask]
              , let delta = dH i
              , oracle mac{ macHash = hash `xorb` delta } ]
```

`findForgedHashes` takes the truncated hash size and the original MAC,
and the oracle takes the forged MAC as an argument.
It returns a list of pairs of the A matrices of modified messages
and the corresponding hashes.

```haskell
findForgedHashes :: Int -> Int -> GCMMACTrunc nonce ad
                 -> (GCMMACTrunc nonce ad -> Bool)
                 -> BitMatrix
                 -> [(BitMatrix,Bytes)]
findForgedHashes r n mac oracle x =
  mapMaybe expandSolution (bitFlipSolutions k n dz x)
 where
  (nonce,ad,ct) = macMessage mac
  blockSize = 16
```

dz is the change in the length block; we just xor the old size and the new size
(the old size, expanded to a full multiple of the block size),
and pack them into a block.

```haskell
  oldSize = numBytes ct
  newSize = blockSize * (oldSize `cdiv` blockSize)
  dz = bigEndian blockSize $ (8 * oldSize) `xor` (8 * newSize)
  lenMatrix = lenAMatrix dz
```

We expand the ciphertext to the full block size
and again split off the power blocks.

```haskell
  splitPowerBlocks _ bs | B.null bs = []
  splitPowerBlocks n bs =
    let (xs,p) = splitEnd blockSize bs
        (rest,r) = splitEnd ((n-1)*blockSize) xs
    in  (p,r) : splitPowerBlocks (2*n) rest
  ct' = ct <> B.replicate (newSize - oldSize) 0
  (pbs,obs) = unzip (splitPowerBlocks 2 ct')
```

We choose k as before, modified by our bit threshold:

```haskell
  k = chooseK r n x
```

If we have a delta that zeros out k rows of T,
then we find the hash that corresponds to the message,
and return it along with the matrix `Ad + mul(dz)`.

```haskell
  expandSolution delta =
```

We reconstruct our ciphertext from the delta as in the last Challenge.

```haskell
    let dbs = chunksOf blockSize delta
        pbs' = zipWith xorb pbs dbs
        ct' = B.concat $ reverse $ interleave pbs' obs
```

We use `hashDeltasFor` to brute-force a hash delta,
given a MAC on the new ciphertext.

```haskell
        mac' = mac{ macMessage = (nonce,ad,ct') }
        mdhash = listToMaybe $ hashDeltasFor k oracle mac'
```

We return dhash and the modified A matrix.

```haskell
    in  (\dhash -> (aMatrix dbs + lenMatrix, dhash)) <$> mdhash
```

Now we can chain together forgeries to find h.
This function takes the oracle as well as a method to procure a new MAC
when we've wrung every bit possible from the existing one.

```haskell
findH :: Monad m
      => Int -> m (GCMMACTrunc nonce ad) -> (GCMMACTrunc nonce ad -> Bool)
      -> m GF
findH r getMAC oracle = nextMAC (initM,initX)
 where
  blockSize = 16
```

`findForgedHashes` returns a list of pairs [(A matrix, hash delta)].
We send this list to `useForgeries` to attempt to improve our M matrix,
or return if we've improved k or extracted all we can from this MAC.
Different MACs have different lengths, so we also have to pass
the number of power blocks in the message.

If we've run out of forged hashes, we return what information we have.

```haskell
  useForgeries _ (m,x) [] = (m,x)
```

Otherwise, we see what we can learn from the forgery.

```haskell
  useForgeries n (m,x) ((ad,dh):ahs) =
```

We know that h satisfies `Ad * h = dh`.
We take the row basis of [dh | Ad]:

```haskell
    let gs = M.toRows . rowBasis $
             M.asColumn (fromBytes dh) M.||| M.takeRows r ad
```

We want to find rows perpendicular to h, i.e. rows where the dh value is zero.
Since dh is the first column, we will have at most one of these rows.

```haskell
        m0 = [ VS.tail r | r <- gs, VS.head r == 0 ]
```

We combine these rows with m to get a new set of vectors orthogonal to h.

```haskell
        m' = rowBasis $ m M.=== M.fromRows m0
        x' = M.fromColumns (kernelBasis m')
```

If we found no new information, we return what we have.
"No new information" means either we have no rows of gs orthogonal to h,
or (as before) our old and new X are the same size.

```haskell
    in  if | null m0 || M.cols x == M.cols x' -> (m,x)
```

Otherwise, we check if we have enough new information to increase k.
If not, we use the next forgery with our new M and X matrices.

```haskell
           | chooseK r n x == chooseK r n x' -> useForgeries n (m',x') ahs
```

Finally, if we improved k we return the new values of M and X
to reformulate our forgeries.

```haskell
           | otherwise -> (m',x')
```

The function `improveK` creates and processes forgeries from a given MAC
until k improves.

```haskell
  improveK mac (m,x) =
    let blockSize = 16
        MAC{ macMessage = (_,_,ct) } = mac
        n = integerLog2 $ fromIntegral $ numBytes ct `cdiv` blockSize
```

We use `findForgedHashes` to create a list of pairs `[(A matrix, dhash)]`
for successfully forged messages,
then call `useForgeries` to extract what information it can
before running out of forgeries or needing to reformulate the matrices.

```haskell
        ahs = findForgedHashes r n mac oracle x
        (m',x') = useForgeries n (m,x) ahs
```

We're done when there's no uncertainty left in h,
i.e. when X is down to one column.
At this point, that single column is just the value of h.

```haskell
    in  if | M.cols x' == 1 -> pure $ convBytes $ M.flatten x'
```

If we have gained no new information at all, we need to bail and try a new MAC.

```haskell
           | M.cols x' == M.cols x -> nextMAC (m',x')
```

Otherwise, we contine to use the current MAC to get more bits.

```haskell
           | otherwise -> improveK mac (m',x')
```

`nextMAC` gets a new MAC once we've extracted all value from the old one.

```haskell
  nextMAC (m,x) = do
    mac <- getMAC
    improveK mac (m,x)
```

Our initial knowledge is nothing; thus, our M matrix is empty
and our X matrix is just the identity.

```haskell
  initM = (0 M.>< 128) []
  initX = M.ident 128
```
