# Solution to Challenge 64

```haskell
{-# LANGUAGE MultiWayIf #-}

module Challenge64
  (
    GCMMACTrunc(..), gcmCTRTrunc, decryptGCMCTRTrunc
  , multMat, aMatrix, tMatrix
  , chooseK
  , findH
  ) where

import Bytes ( HasBytes(..), Bytes, convBytes, chunksOf, xorb, splitEnd )
import BitMatrix ( BitMatrix(..), rowBasis, kernelBasis )
import GCM ( GF, gcmCTR, mkGCMMACHash )
import AES ( decryptCTR )
import Hash ( MAC(..) )
import Util ( interleave )

import Data.Bits ( Bits(..) )
import Data.List ( unfoldr, foldl' )

import qualified Numeric.LinearAlgebra as M
import qualified Data.ByteString as B
import qualified Control.Monad.Random as R
```

We create a version of GCMCTR that truncates the hash to some number of bytes.

```haskell
type GCMMACTrunc nonce ad = MAC (nonce,ad,Bytes) Bytes

gcmCTRTrunc :: (HasBytes key, HasBytes nonce, HasBytes ad, HasBytes text)
            => Int -> key -> nonce -> ad -> text -> GCMMACTrunc nonce ad
gcmCTRTrunc len key nonce ad text =
  let MAC{ macMessage = msg, macHash = hash } = gcmCTR key nonce ad text
  in  MAC{ macMessage = msg, macHash = B.take len $ toBytes hash }

decryptGCMCTRTrunc :: (HasBytes key, HasBytes nonce, HasBytes ad)
                   => Int -> key -> GCMMACTrunc nonce ad -> Maybe (ad,Bytes)
decryptGCMCTRTrunc len key mac =
  let MAC{ macMessage = (nonce,ad,ct), macHash = bs } = mac
      Just pt = decryptCTR key nonce ct
      hash = mkGCMMACHash key (nonce,ad,ct)
  in  if bs == B.take len (convBytes hash)
      then Just (ad,pt)
      else Nothing
```

## Matrices corresponding to group operations

`gfX` is the polynomial X in GF(2^128),
i.e. the bit string with only the second bit set.

```haskell
gfX :: GF
gfX = 1 `shiftR` 1
```

Multiplication by an element in GF(2^128) corresponds to
multiplication by a 128x128 matrix.

```haskell
multMat :: GF -> BitMatrix
multMat p =
```

The powers of `gfX` are the elements which have exactly one bit set.
We find the product of `p` with all of them.

```haskell
  let cols = take 128 $ iterate (*gfX) p
```

We convert the GF elements to bit vectors and pack them together.

```haskell
  in  M.fromColumns (map convBytes cols)
```

Squaring an element in GF(2^128) is a linear operation (as in any GF(2^k))
so we can also represent it by a matrix.

```haskell
sqrMat :: BitMatrix
sqrMat =
```

Column i is the square of x^i.

```haskell
  let cols = take 128 $ iterate (* (gfX * gfX)) 1
  in  M.fromColumns (map convBytes cols)
```

We can turn squaring into taking any power of 2.

```haskell
powMats :: [BitMatrix]
powMats = iterate (<>sqrMat) (M.ident 128)
```

We have matrices for all of the multipliers corresponding to single-bit vectors.

```haskell
multBits :: [BitMatrix]
multBits = map (multMat . bit) [127,126..0]
```

## Attacking truncated GCM

The attack involves creating forgeries
(which is made easier by the limited MAC size),
then using those forgeries to limit the possible values for h
to smaller and smaller subspaces;
each limitation makes finding forgeries easier and easier,
thus reducing the space further,
until we determine the exact value of h and can thus forge at will.

Given ciphertext blocks ci, creating a forgery that validates
means creating blocks c'i that hash to the same (truncated) MAC.
The MAC is of the form

    s + z*h + c1 * h^2 + c2 * h^3 + ...

(where s is the nonce and z is the size descriptor,
and the cis are numbered from back to front).
Taking h to the power of any power of 2 is a linear operation,
so we'll leave most cis the same and change only c1, c3, c7, etc.
Say dj = (c(2^j-1) + c'(2^j-1)); then we want to find ds such that

    0 = sum (d_j * h^(2^j))
	  = sum (d_j * sqr^j * h)
	  = sum ( mul(d_j) * sqr^j ) * h.

We call the matrix `Ad = sum ( mul(d_j) * sqr^j )`;
we then want to find `Ad * h = 0`.

'aMatrix' produces this matrix;
the function takes a list of d blocks (dj is the XOR applied to c(2^j-1))
and produces the matrix Ad such that
Ad * h is the XOR between the original hash and the new one.

```haskell
aMatrix :: [Bytes] -> BitMatrix
aMatrix ds = sum
  [ multMat (fromBytes di) <> (powMats !! i)
  | (i,di) <- zip [1..] ds ]
```

There is also a linear relationship between the values of the ds
and the entries in Ad.
We can thus create a linear map T from the ds to the entries in Ad;
or, rather, to the entries of the first r rows of Ad.
Call this submatrix Bd.
The map is from the space of bitflips in our power blocks (dimension 128n)
to the space of the rows of Bd (dimension 128r).
We also include the ability to multiply Bd by another matrix X;
see below for why we want to do this.

```haskell
tMatrix :: Int -> Int -> BitMatrix -> BitMatrix
tMatrix r n x = mat
 where
```

The matrix `Bd * X` is built from

    Ad * X = sum ( mul(d_j) * sqr^j * X).

Changing a single bit (bit i of `d_k`, say)
therefore has the effect of adding to this the matrix

    bitmul_i * sqr^k * X.

The matrix T has one column corresponding to each i and k.

```haskell
  powXs = [ p <> x | p <- tail powMats ]
  colData = [ mul <> powX | powX <- take n powXs
                          , mul <- multBits ]
```

We simply have to cut the columns to the right length
and pack them together into a `BitMatrix`.

```haskell
  colSize = M.cols x * r
  cols = map (M.subVector 0 colSize . M.flatten) colData
  mat = M.fromColumns cols
```

Note that the kernel of T is the space of ds that set the Bd matrix to all zeros;
these bit changes will then create Bds that satisfy `Bd * h = 0`,
so they will have no affect on the MAC.
However, T has 128r rows and 128n columns
(with r = MAC size about 32, and n = log message size hopefully much less)
so its kernel is very likely to be zero!

Look however at only Tk, the top part of the T matrix;
say, the top k = 128k' rows.
These correspond to the top k' rows of the Bd matrices,
so the kernel of Tk is going to be all bit changes that leave the
first k' bits of the MAC unchanged.
How does this help us?
Well, if the oracle is only considering the first r bits of the MAC,
then if we are trying to brute force bit changes that leave them unchanged
we will have to try about 2^r altered messages.
But elements of the kernel of Tk all leave the first k' bits the same,
so choosing from only these elements will mean we only have to
try about 2^(r-k') of them to get a MAC collision.

Once we've found a collision,
we can use that information to narrow our search.
A valid forgery gives us a bitflip string d' that leaves the MAC unaffected,
i.e. with Bd' * h = 0; h is thus in the kernel of Bd'.
Since Bd' has 128 columns and k' zero rows,
its kernel has dimension l ~= 128 - k',
and we can confine h to an l-dimensional subspace which has basis X.
But now we can write any possible value of h as X * h', where h' has l bits;
this means that forgeries are ds that fulfil Cd * h' = 0,
where Cd = Bd * X has r rows and l columns,
which in turn means that the new T matrix has r*l rows.

The number of rows in T is important because it lets us
reduce the search space for finding forgeries.
Remember that we look for forgeries in the kernel of Tk, the first k rows of T.
How do we choose k?
We have two limitations on its value.
First, we have to have some kernel at all,
so Tk needs to be wider than it is high; that is, k < 128n.
In addition, though, we can't set every row of Cd to zero;
then the forgery will be accepted, but we won't learn anything new about h,
which is the entire point here.
We have to leave at least one entire row of Cd nonzero; that is, k <= l*(r-1).
Therefore, we choose k/l = min ((128n - 1)/l) (r-1).

```haskell
chooseK :: Int -> Int -> BitMatrix -> Int
chooseK r n x = min ((128*n - 1) `div` M.cols x) (r-1)
```

`bitFlipBasis` finds a basis for the bit flips to look for,
given the size of the MAC r and the number of power blocks n.

```haskell
bitFlipBasis :: Int -> Int -> BitMatrix -> [Bytes]
bitFlipBasis r n x = map toBytes basis
 where
  k = chooseK r n x
  tk = tMatrix k n x
  basis = kernelBasis tk
```

Any single vector of bit flips within the search space
is a sum of entries from the basis;
its coordinate is a sequence of bits, one per basis element.
The function `bitFlipVec` takes a basis and a sequence of bits
(provided as an `Integer`)
and returns the vector with that coordinate.

```haskell
bitFlipVec :: [Bytes] -> Integer -> Bytes
bitFlipVec basis v = foldl' xorb noFlips elts
 where
```

We first grab the individual bits from the `Integer`.

```haskell
  bits = unfoldr (\n -> case n `quotRem` 2 of
                     (0,0)  -> Nothing
                     (n2,1) -> Just (True,n2)
                     (n2,0) -> Just (False,n2)) v
```

This lets us select the basis elements corresponding to `True` bits.

```haskell
  elts = [ b | (True,b) <- zip bits basis ]
```

No flips at all corresponds to a vector of all zero bits.

```haskell
  noFlips = B.replicate (B.length $ head basis) 0
```

Now we can look for possible forgeries, in a principled way.
The function `findForgeries` takes an oracle to tell us
whether a given bitflip vector validates OK.

```haskell
findForgeries :: R.MonadRandom m
              => Int -> Int -> (Bytes -> Bool) -> BitMatrix
              -> m [Bytes]
findForgeries r n oracle x = processCoordinates <$> R.getRandomRs (1,kernelSize)
 where
```

We find the basis of the bit flips which preserve the first k bits of the hash.

```haskell
  basis = bitFlipBasis r n x
```

We also know the total size of the space of bit flips.

```haskell
  kernelSize = 2^(length basis)
```

We will generate and process random coordinates (`Integer`s).
We find the bit flip corresponding to each coordinate,
then filter out just those which pass the oracle.

```haskell
  processCoordinates = filter oracle . map (bitFlipVec basis)
```

Now we can chain together forgeries to find h.

```haskell
findH :: R.MonadRandom m => Int -> (Bytes -> Bool) -> Bytes -> m GF
findH r oracle ct = improveK (initM, initX)
 where
  blockSize = 16
```

Because the only thing we can safely manipulate are the blocks
of power-of-two index,
we have to split the ciphertext into pieces and separate them.
`splitPowerBlocks` splits blocks off from the end of the ciphertext,
doubling the length between blocks each time.
It returns a list `[(power block i, 2^i - 1 blocks)]`.

```haskell
  splitPowerBlocks _ bs | B.null bs = []
  splitPowerBlocks n bs =
    let (xs,p) = splitEnd blockSize bs
        (rest,r) = splitEnd ((n-1)*blockSize) xs
    in  (p,r) : splitPowerBlocks (2*n) rest
  (pbs,obs) = unzip (splitPowerBlocks 2 ct)
  n = length pbs
```

We also have to be able to apply bit flips to the power blocks,
reassemble the ciphertext, and validate it with the oracle.

```haskell
  oracle' delta = oracle ct'
   where
```

Cut the bit flip into blocks and apply it to the power blocks.

```haskell
    dbs = chunksOf blockSize delta
    pbs' = zipWith xorb pbs dbs
```

Reconstruct the ciphertext,
remembering that our power blocks are in *reverse* order.

```haskell
    ct' = B.concat $ reverse $ interleave pbs' obs
```

Our progress toward finding h is the subspace of vectors orthogonal to h;
we store these as the rows of matrix M.
The matrix X is constructed from the kernel of M;
we can restrict h to a representation h' in a subspace
whose dimension is the number of columns in X,
then multiply X by h' to transform to its full 128-bit representation.

Once we have generated forgeries with a given M matrix,
we try to use it to improve our knowledge and reduce the uncertainty about h.

```haskell
  useForgeries (m,x) (d:ds) =
```

Since we have a forgery, we know that `Ad * h = 0`,
i.e. the first r rows of Ad are orthogonal to h.

```haskell
    let ar = M.takeRows r $ aMatrix $ chunksOf blockSize d
```

We adjoin these rows to our rows m and reduce to get
a new set of row vectors orthogonal to h.

```haskell
        m' = rowBasis $ m M.=== ar
```

Recompute X:

```haskell
        x' = M.fromColumns (kernelBasis m')
```

The function `findForgeries` produces forgeries where we know
that at least k bits of the hash are correct;
but this means that (r-k) bits are chosen randomly,
so we have to try about 2^(r-k) forgeries to get one that works.
Thus, we want to start generating new forgeries as soon as
we can increase the value of k.
However, even if we improve our knowledge about h,
that will not necessarily improve k, which we recall is

    k = min ((128*n - 1) `div` M.cols x) (r-1).

Reducing the number of columns of X will not necessarily reduce k.
Since formulating the T matrix is quite computationally expensive,
we would therefore like to potentially process many forgeries
until either k improves or our sample space becomes too large
relative to our knowledge.

A forgery that doesn't improve our knowledge about h
is an indication that the sample space for our forgeries
is becoming too large.
We thus choose to reformulate the forgeries if this happens.

```haskell
    in  if | M.cols x' == M.cols x -> (m,x)
```

Even if we improved X, if we couldn't increase k we'll try the next forgery,
albeit with our updated M and X.

```haskell
           | chooseK r n x == chooseK r n x' -> useForgeries (m',x') ds
```

Finally, if we improved k we return the new values of M and X.

```haskell
           | otherwise -> (m',x')
```

The function `improveK` creates and processes forgeries
until k improves.

```haskell
  improveK (m,x) = do
    (m',x') <- useForgeries (m,x) <$> findForgeries r n oracle' x
```

We're done when there's no uncertainty left in h,
i.e. when X is down to one column.
At this point, that single column is just the value of h.

```haskell
    if M.cols x' == 1
    then pure $ convBytes $ M.flatten x'
```

Otherwise, we reformulate and continue.

```haskell
    else improveK (m',x')
```

Our initial knowledge is nothing; thus, our M matrix is empty
and our X matrix is just the identity.

```haskell
  initM = (0 M.>< 128) []
  initX = M.ident 128
```
