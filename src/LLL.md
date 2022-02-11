# Lattice basis operations

Lattice basis operations and the
[Lenstra-Lenstra-Lovász algorithm](https://en.wikipedia.org/wiki/LLL_algorithm).

```haskell
{-# LANGUAGE DeriveFunctor #-}

module LLL
  (
    RowVec(..)
  , lll
  ) where

import Data.List ( foldl' )
import Data.Maybe ( fromJust )

import qualified Data.Vector as V
```

First, some data structures and functions to help us
work with lattice elements.

A `RowVec` is an element of the lattice
or an underlying vector field.
The coordinates of the input rows are `Integer`s;
the coordinates of orthogonalized rows computed
during the LLL algorithm are `Rational`.

```haskell
newtype RowVec a = RowVec { unRowVec :: V.Vector a }
  deriving (Eq, Ord, Show, Functor)
```

We have some operations for combining the row vectors:
`axpy` (a times x plus y) adds a scalar times the first vector
to the second vector.

```haskell
axpy :: Num a => a -> RowVec a -> RowVec a -> RowVec a
axpy a (RowVec xs) (RowVec ys) = RowVec $ V.zipWith (\x y -> a*x + y) xs ys
```

`axpby` (a times x plus b times y) adds a scalar times the first vector
to a scalar times the second vector.

```haskell
axpby :: Num a => a -> RowVec a -> a -> RowVec a -> RowVec a
axpby a (RowVec xs) b (RowVec ys) = RowVec $ V.zipWith (\x y -> a*x + b*y) xs ys
```

`dotp` computes the dot product of two vectors.

```haskell
dotp :: Num a => RowVec a -> RowVec a -> a
dotp (RowVec xs) (RowVec ys) = sum $ V.zipWith (*) xs ys
```

## The Lenstra-Lenstra-Lovász algorithm

The LLL implementation is based on
[the algorithm described on Wikipedia](https://en.wikipedia.org/wiki/LLL_algorithm#LLL_algorithm_pseudocode)
and the C++ implementation in [arageli](http://arageli.org).
It was originally implemented in a very imperative style in the ST monad,
but once I realized it didn't require random access to its data rows,
I rewrote it in a more idiomatic Haskell style.

The imperative algorithm maintains an array of data rows
and the index of the currently explored row.
In a more functional style, this is just a
[list zipper](https://en.wikipedia.org/wiki/Zipper_(data_structure)).
Since the currently explored row is never 0,
this version keeps two rows in focus: the *focus* row and its
immediate predecessor, the *candidate*.

```haskell
data Zipper a = Z
  { zPrev :: [a]
  , zCand :: a
  , zFocus :: a
  , zNext :: [a] }
  deriving (Eq,Ord,Show)
```

Moving left in the zipper pushes the focus row onto `zNext`,
the candidate into focus, and makes the head of `zPrev` into the candidate.

```haskell
zLeft :: Zipper a -> Maybe (Zipper a)
zLeft (Z [] _ _ _) = Nothing
zLeft (Z (p:ps) c f ns) = Just $ Z ps p c (f:ns)
```

Moving right does the opposite.

```haskell
zRight :: Zipper a -> Maybe (Zipper a)
zRight (Z _ _ _ []) = Nothing
zRight (Z ps c f (n:ns)) = Just $ Z (c:ps) f n ns
```

We can find an orthogonal basis of a vector space using the
[Gram-Schmidt process](https://en.wikipedia.org/wiki/Gram–Schmidt_process);
however, this requires combining fractional parts of our input vectors.
In a lattice, we can only combine integral parts of the vectors.
The LLL algorithm performs a Gram-Schmidt orthogonalization of the vectors
as elements of the underlying vector space,
then uses the orthogonalized components to both
derive the lattice additions and subtractions to use,
and to determine when the algorithm has come "close enough" to the minimal basis.

`LLLData` describes the information known about a given row in the LLL reduction.

```haskell
data LLLData = LLLData
  {
```

`lllVec` is the current reduction of the row
(corresponding to b in the Wikipedia algorithm);

```haskell
    lllVec :: !(RowVec Integer)
```

`lllGS` is the Gram-Schmidt reduction of the row, over the rationals
(b* on Wikipedia);

```haskell
  , lllGS  :: !(RowVec Rational)
```

`lllMu` are the coefficients of the projection of b on vectors b*_j
(μ on Wikipedia).

```haskell
  , lllMu  :: !(V.Vector Rational) }
  deriving (Eq,Ord,Show)
```

`computeGS` computes the initial Gram-Schmidt reduction of the next vector,
given the `LLLData` of all of the previous vectors.

```haskell
computeGS :: RowVec Integer -> [LLLData] -> LLLData
computeGS v gs =
  let v' = fmap toRational v
```

This is a simple Gram-Schmidt orthogonalization, without any normalization.
We perform a fold over the previous vectors gs,
finding the projection coefficient mu of
the previous vector on the original vector,
then subtracting that projection from our orthogonalized version bk.

```haskell
      (bk,muks) = foldr reduceBMu (v',[]) gs
      reduceBMu LLLData{ lllGS = bj, lllMu = muj } (bk,muks) =
```

The projection of bj on v is

    mu_kj = (bj . v) / (bj . bj);

the denominator is the first entry of node j's mu vector.

```haskell
        let mukj = (bj `dotp` v') / V.head muj
```

The mu vector for this node has the projection prepended:

```haskell
            muks' = mukj : muks
```

We subtract the projection from bk:

```haskell
            bk' = axpy (-mukj) bj bk
```

and return the new values of bk and muks:

```haskell
        in  (bk',muks')
```

Finally, we return the `LLLData` make from the pair (bk,muks).
The only change we have to make is to prepend `(bk . bk)` to the mu vector.

```haskell
 in LLLData{ lllVec = v, lllGS = bk
           , lllMu = V.fromList (dotp bk bk : muks) }
```

`reduceSize` reduces the size of a given vector by
subtracting off the largest integral amount possible
of another given basis vector.
Most of its work is done to update the projection coefficients;
to this end, we must also pass as an argument
the index of the subtracted vector in the mu array.

```haskell
reduceSize :: LLLData -> (Int,LLLData) -> LLLData
reduceSize gk (l,gl) = gk{ lllVec = vk', lllMu = muk' }
 where
  LLLData{ lllVec = vk, lllMu = muk } = gk
  LLLData{ lllVec = vl, lllMu = mul } = gl
```

`q` is the integral amount of bl to subtract from bk.

```haskell
  q = round (muk V.! l)
  vk' = axpy (-q) vl vk
```

`reduceI` computes the new value at index i of the muk array
from the old value `mki`.
The particular action we take depends on whether the projected vector
comes before or after the vector gl in the list.

```haskell
  muk' = V.imap reduceI muk
  reduceI i mki = case compare i l of
```

Vectors with low indexes come after gl,
so they are already perpendicular to it (and thus to any multiples of it);
their projection coefficients are thus unaffected.

```haskell
                    LT -> mki
```

The projection coefficient of gl just has
the integer q subtracted from it.

```haskell
                    EQ -> mki - fromIntegral q
```

The projection coefficents after l have to subtract off
the integer q times the corresponding coefficient of bl,
i.e. since bk -> bk - q * bl,

    proj(b_k,b_i) -> proj(b_k,b_i) - q * proj(b_l,b_i).

```haskell
                    GT -> mki - fromIntegral q * (mul V.! (i-l))
```

The `lll` routine performs a basis reduction on the input vectors.

```haskell
lll :: [RowVec Integer] -> [RowVec Integer]
lll (r1:r2:rs) = go rs initZ
 where
```

We perform the computation by moving through the list of `LLLData`
using our list zipper.
Elements to the left of the current focus are our "active" rows
(corresponding to i<k in the Wikipedia algorithm);
elements to the right are "standby" rows.
We also maintain `rs`, the list of input rows which have not yet been processed;
when we reach the end of the zipper, we can add one element of rs.
We make this distinction because adding an input row requires us to
not only compute its complete Gram-Schmidt reduction,
but to maintain this reduction under all further row operations.
We can save a great deal of effort if we don't begin to work with any row
until the rows before it are as minimal as possible.

Our initial set of active rows is built from
the Gram-Schmidt orthognalizations of the first two rows in the input.

```haskell
  initZ =
    let g1 = computeGS r1 []
    in  Z [] g1 (computeGS r2 [g1]) []
```

Ideally, we would like to sort our active rows smallest-first,
but in order for the algorithm to work in polynomial time,
we must allow for slight increases in length between successive elements.
We swap two elements if they fulfil the *Lovász criterion*:

    d b1^2 <= b0^2 + mu01^2 * b1^2

for some d < 1. (The Challenge suggests we set it to 0.99 and forget about it.)

```haskell
  delta = 0.99
```

We can check the condition from the mu vectors of b0 and b1:

```haskell
  isLovasz LLLData{lllMu = mu0} LLLData{lllMu = mu1} =
    V.head mu0 < (delta - (mu0 V.! 1)^2) * V.head mu1
```

The main workhorse is the `go` function.
It first checks if we can reduce the size of the focus vector
by subtracting an integer multiple of the candidate vector.
This can be done exactly when the absolute value of
the projection coefficient mu is greater than 1/2,
i.e. when it does not round to zero.

```haskell
  go rs z@(Z gs c f hs)
    | round (lllMu f V.! 1) /= 0 = go rs z{ zFocus = reduceSize f (1, c) }
```

We otherwise check the Lovasz criterion to see if we should swap the two vectors.
If we do so, then we shift left (if possible) and recurse to `go`
to try to reduce against a new candidate.

```haskell
    | isLovasz f c =
      let z' = exchangeRows z
      in  go rs $ maybe z' id (zLeft z')
```

If the ordering is fine, then we reduce the size of the focus vector
as much as we can and move on to the next row.

```haskell
    | otherwise =
      let f' = foldl' reduceSize f (zip [2..] gs)
```

The next row might be the next element of the standby rows:

```haskell
      in  case (rs, zRight z{ zFocus = f' }) of
            (_, Just z') -> go rs z'
```

or, if there are no more standby rows, the next unprocessed row:

```haskell
            (r:rs', _) -> go rs' (Z (c:gs) f' (computeGS r $ f':c:gs) [])
```

or, if we're out of new rows, we're done and can return.

```haskell
            _ -> reverse (map lllVec $ f':c:gs)
```

To exchange the focus and candidate rows, we have to
update all of the projection coefficients in the two swapped rows
as well as all of the standby rows.

```haskell
  exchangeRows (Z gs c f hs) = Z gs f' c' hs'
   where
    LLLData{ lllGS = bk, lllMu = muk } = f
    LLLData{ lllGS = bk1, lllMu = muk1 } = c
```

The algebra is a little involved but not especially interesting.

```haskell
    [mkk,mu] = V.toList (V.take 2 muk)
    mk1k1 = V.head muk1

    b2   = mkk + mu*mu*mk1k1
    mkk1 = mu * mk1k1 / b2

    bk'  = axpy mu bk1 bk
    muk' = b2 `V.cons` V.drop 2 muk
    f'   = f{ lllGS = bk' , lllMu = muk' }

    bk1' = axpby (-mkk1) bk (mkk/b2) bk1
    muk1'= (mk1k1 * mkk / b2) `V.cons` (mkk1 `V.cons` V.tail muk1)
    c'   = c{ lllGS = bk1', lllMu = muk1' }
```

We also have to swap the projection coefficients in the standby rows.

```haskell
    subUp i hu@LLLData{ lllMu = mui } =
      let t = mui V.! i
          mik1 = mui V.! (i+1)
          mik = mik1 - mu*t
          mui' = mui V.// [(i,mik), (i+1, t + mkk1 * mik)]
      in  hu{ lllMu = mui' }
    hs' = zipWith subUp [1..] hs
```
