# Matrix operations for bit values

This module defines matrix operations for bit values,
used in Challenges 64 and 65.

```haskell
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module BitMatrix
  (
    BitVector, BitMatrix
  , gaussElim
  , rowBasis, kernelBasis
  , solution, solutionSpace
  ) where

import Bytes ( HasBytes(..) )
import Bytes.Integral ( bigEndian )
import Util ( cdiv )

import Data.Bits ( Bits(..) )
import Data.List ( foldl' )

import Numeric.LinearAlgebra hiding ((<>))
import Numeric.LinearAlgebra.Devel

import qualified Data.ByteString as B
import qualified Data.Vector.Storable as V
```

Our matrices of bit values are defined via the
[hmatrix](https://hackage.haskell.org/package/hmatrix) package.

```haskell
type Bit = I ./. 2
type BitVector = Vector Bit
type BitMatrix = Matrix Bit
```

We can convert between bit vectors and `Bytes`.

```haskell
instance HasBytes BitVector where
```

There are eight bits to a byte.

```haskell
  numBytes v = size v `cdiv` 8
```

To convert from `Bytes`, we create a list of all of the bit values
then turn that into a `Vector`.

```haskell
  fromBytes bs = V.fromList [ if testBit b n then 1 else 0
                            | b <- B.unpack bs, n <- [7,6..0] ]
```

To convert to `Bytes`, we turn the vector into an `Integer`
then convert that.

```haskell
  toBytes v = bigEndian (numBytes v) $
              foldl' (\n b -> 2*n + fromIntegral b) (0 :: Integer) $
              V.toList v
```

## Gaussian elimination

The `gaussElim` function uses Gaussian elimination to bring a `BitMatrix` to
[reduced row echelon form](https://en.wikipedia.org/wiki/Row_echelon_form).

```haskell
gaussElim :: BitMatrix -> BitMatrix
```

The matrix is modified in the ST monad,
starting Gaussian elimination at the top-left corner.

```haskell
gaussElim mat = runSTMatrix $ thawMatrix mat >>= gelim 0 0
 where
  nr = rows mat
  nc = cols mat
```

During Gaussian elimination, we work our way downward and to the right,
keeping track of our current row and column.

First we check if we've handled all of the rows;
if we have moved beyond the bounds of the matrix, we're done.

```haskell
  gelim r c mat
    | r == nr || c == nc = pure mat
```

Otherwise, each step tries to ensure that the only 1 in column c is in row r.

```haskell
    | otherwise = do
```

`colVals` is a list of all of the values in column c.

```haskell
      colVals <- sequence [ readMatrix mat r' c | r' <- [0..nr-1] ]
```

We use it to generate oneRows: a list of rows which have a one in column c.

```haskell
      let oneRows = [ r' | (r',1) <- zip [0..] colVals ]
```

If there are no ones in the column at row r or below,
then the variable corresponding to this column is not independent,
and we move on to the next column.

```haskell
      case dropWhile (<r) oneRows of
        [] -> gelim r (c+1) mat
```

Otherwise, we take the first row with a one and swap it into row r.

```haskell
        r':_ -> do
          if r /= r'
            then rowOper (SWAP r r' $ FromCol c) mat
            else pure ()
```

We then subtract it from every other row that has a one in the column,
thus removing every other entry in this column.

```haskell
          sequence_ [ rowOper (AXPY 1 r r'' $ FromCol c) mat
                    | r'' <- oneRows, r'' /= r' ]
```

We can then move to the next row and column.

```haskell
          gelim (r+1) (c+1) mat
```

## Matrix operations

Basis for the rows of the matrix.
This just performs Gaussian elimination and removes any zero rows.

```haskell
rowBasis :: BitMatrix -> BitMatrix
rowBasis = fromRows . filter (V.any (/=0)) . toRows . gaussElim
```

Basis for the kernel of the matrix;
that is, the basis for vectors v such that `A * v = 0`.

```haskell
kernelBasis :: BitMatrix -> [BitVector]
kernelBasis mat = map flatten kernelBlocks
 where
```

We compute the row reduction of the matrix `(A^T | I)`:

```haskell
  gel = gaussElim $ tr mat ||| ident (cols mat)
```

Split the row reduction into blocks, two per row:

```haskell
  blocks = toBlocks (replicate (cols mat) 1) (rows mat : cols mat : []) gel
```

We want only the rows of the right block for which the left block is zero.

```haskell
  zeros = (1 >< rows mat) $ repeat 0
  kernelBlocks = [ c | [r,c] <- blocks, r == zeros ]
```

Find a solution for a linear system.
We find the row reduction of the matrix `(A | b)`;
each nonzero row corresponds to a linear equation of the form

    x_i1 + x_i2 + ... + x_ik = y_i,

and we choose the solution `x_i1 = y_i, x_i{2..k} = 0`.

```haskell
solution :: BitMatrix -> BitVector -> Maybe BitVector
solution mat vec = let
```

`gs` is a list of the reduced rows of the matrix `(A | b)`.

```haskell
  gs = toRows . gaussElim $ mat ||| asColumn vec
```

`leadingOnes` is a list of the index of the first one in each row.

```haskell
  leadingOnes = map (V.findIndex (==1)) gs
```

`oneIndexes` is a list of all of the leading ones
with their corresponding value in the last column.

```haskell
  oneIndexes = [ (i,b) | (Just i, b) <- zip leadingOnes (map V.last gs) ]
```

If the leading one in any row is the last column,
i.e. the column corresponding to b,
then we have the equation 0 = 1; there is thus no solution.

```haskell
 in if any (== Just (cols mat)) leadingOnes
    then Nothing
```

Otherwise, the solution we choose is a vector of zeros,
except for ones corresponding to the leading ones in each row
with a corresponding one in the last column.

```haskell
    else Just $ V.replicate (cols mat) 0 V.// oneIndexes
```

Find a solution space for a linear system.
This consists of a particular solution (maybe zero)
and the basis of the kernel of the matrix.

```haskell
solutionSpace :: BitMatrix -> BitVector -> Maybe (BitVector,[BitVector])
solutionSpace mat vec = do
  singleSolution <- solution mat vec
  pure (singleSolution , kernelBasis mat)
```
