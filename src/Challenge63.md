# Solution to Challenge 63

```haskell
module Challenge63
  (
    forgeGCM
  , findGCMKey
  ) where

import Bytes ( HasBytes(..), Bytes, convBytes, chunksOf )
import Bytes.Integral ( bigEndian )
import Hash ( MAC(..) )
import GCM ( GF, GCMMAC, gcmHash )
import Polynomial ( Poly(..), toPoly, polyX, polyDegree, polyCoeff, polyEval
                  , polyQuot, polyModPow, polyGCD, polyDerivative )
import Util ( cdiv )
import Random ( randomGF )

import Control.Monad ( replicateM, forM )
import Data.Bifunctor ( first )
import Data.List ( foldl' )
import Data.Maybe ( mapMaybe )

import qualified Data.IntMap as IM
import qualified Control.Monad.Random as R
import qualified Data.ByteString as B
```

We break GCMCTR with repeated nonces, using the fact that a GCMCTR 'hash'
is the evaluation of a polynomial (with the nonce as constant term)
at the key. To do this, we have to factor a polynomial over GF(2^128).

## Polynomial factorization over finite fields

After [the entry on Wikipedia](https://en.wikipedia.org/wiki/Factorization_of_polynomials_over_finite_fields).

First, we make the polynominal *monic*
(i.e. have a highest-degree coefficient of 1).
This function returns both the original coefficient and the monic polynomial.

```haskell
polyMonic :: (Eq a, Fractional a) => Poly a -> (a, Poly a)
polyMonic p = case polyDegree p of
```

The degree of a zero polynomial is -1.

```haskell
  -1 -> (1,p)
```

Otherwise, we can look up the coefficient
and divide all of the other coefficients by it.

```haskell
  n  ->
    let Just an = polyCoeff p n
    in  (an, fmap (/an) p)
```

The function `monicGCD` finds the monic version of the GCD of two polynomials.

```haskell
monicGCD :: (Eq a, Fractional a) => Poly a -> Poly a -> Poly a
monicGCD a b = snd . polyMonic $ polyGCD a b
```


### Square-free factorization

This is based on the fact that every multiple root of p
is a root (with one less multiplicity) of p' (the derivative of p),
while the other roots of p' do not correspond to roots of p.
   
We will perform an iteration, pulling squares out of the polynomial
and factoring out non-square pieces.
The iteration information will be kept in `SFFData`:
   
```haskell
data SFFData a = SFFData
```

The product of all unprocessed factors is in `sffRemain`,
while the polynomial divided by the unprocessed and processed factors
is in `sffProduct`.

```haskell
  { sffProduct :: Poly a
  , sffRemain :: Poly a
```

The factors themselves are in the list `sffFactors`,
pairing a polynomial factor with its multiplicity.

```haskell
  , sffFactors :: [(Int, Poly a)]
```

The multiplicity of the next factors to extract is `sffDegree`.

```haskell
  , sffDegree :: Int }
  deriving (Eq,Show)
```

The iteration pulls out factors of increasing multiplicity.

```haskell
sffFactorOne :: (Eq a, Fractional a) => SFFData a -> SFFData a
sffFactorOne sffd =
```

At each step of the iteration, we take the GCD of
our current product and the remainder of the GCD.

```haskell
  let y = monicGCD (sffProduct sffd) (sffRemain sffd)
      product' = sffProduct sffd `polyQuot` y
      remain' = sffRemain sffd `polyQuot` y
```

Any part of the product that is not in the GCD
is a square-free factor of multiplicity i.

```haskell
      factor = (sffDegree sffd, product')
```

We have now taken out factors of the next-higher degree.

```haskell
  in  SFFData{ sffProduct = y
             , sffRemain = remain'
             , sffFactors = factor : sffFactors sffd
             , sffDegree = sffDegree sffd + 1 }
```

After extracting all factors, we have to look at the remaining part;
this contains all factors whose multiplicity is a multiple of p.

```haskell
sffPFactors :: (Eq a, Fractional a) => (Int,Int) -> SFFData a -> [(Int, Poly a)]
```

The simplest case is when there are no such factors;
the SFF is then just the factors we extracted during the iteration.

```haskell
sffPFactors _ SFFData{ sffRemain = 1, sffFactors = rs } = rs
```

Otherwise, we compute the new factors and add them to
the factors we've already found.

```haskell
sffPFactors (p,e) sffd = factors ++ sffFactors sffd
 where
  SFFData{ sffRemain = Poly c } = sffd
```

An `sffRemain /= 1` will be of the form

    an*(x^p)^n + ... + a0.

Since taking the pth power is linear in GF(p^k),
we can take its pth root easily:
substitute x -> x^(1/p) and ai -> ai^( p^(e-1) ).

```haskell
  pe1 = p^(e-1)
  reducedC = Poly $ IM.mapKeysMonotonic (`div` p) $ IM.map (^pe1) c
```

We then factorize this pth root
and multiply the multiplicities of the new factors by p.

```haskell
  sffs = polySFF (p,e) reducedC
  factors = map (first (*p)) sffs
```

Now we can perform the square-free factorization.
We return the list of all factors of unique multiplicities.

```haskell
polySFF :: (Eq a, Fractional a) => (Int,Int) -> Poly a -> [(Int,Poly a)]
polySFF pe f = allFactors
 where
```

We first take the GCD of f and its derivative. This will be a product,
without multiplicity, of all of the irreducible factors of f.

```haskell
  c0 = monicGCD f (polyDerivative f)
```

Our iteration thus starts with

```haskell
  initData = SFFData{ sffProduct = f `polyQuot` c0, sffRemain = c0
                    , sffFactors = [], sffDegree = 1 }
```

We end the iteration when we have extracted all of the factors
whose multiplicity is not a multiple of p;
at this point, the product of the remaining factors is 1.

```haskell
  unreduced = head $ dropWhile ((/=1) . sffProduct) $
              iterate sffFactorOne initData
```

Finally, we add in the factors whose multiplicity is a multiple of p.

```haskell
  allFactors = sffPFactors pe unreduced
```

### Distinct-degree factorization

This splits a square-free polynomial into pieces
such that each is the product of irreducible polynomials
of the same degree.

The algorithm is based on the fact that, in a finite field of size q,
the polynomial (x^ (q^i) - x) is the product of
every irreducible polynomial of degree i and below.
   
We will once again perform an iteration,
extracting the powers of each degree in turn.
We again have an iteration data structure:
   
```haskell
data DDFData a = DDFData
```

`ddfRemain` contains the unprocessed part of our polynomial.

```haskell
  { ddfRemain :: Poly a
```

`ddfFactors` contains the factors we have so far extracted,
tagged by their degree.

```haskell
  , ddfFactors :: [(Int, Poly a)]
```

`ddfDegree` is the degree of the last factors we tried to extract,
and `ddfXq` contains the polynomial `x^(q^ddfDegree)`.

```haskell
  , ddfDegree :: Int
  , ddfXq :: Poly a }
  deriving (Eq,Show)
```

At each step of the iteration, we extract factors of the next degree.
We are carrying along x^(q^i) and take another power of it,
before taking the GCD g of (x^( q^i ) - x)
and the remaining part of the polynomial f.

```haskell
ddfNextDegree :: (Eq a, Fractional a)
              => Integer -> Poly a -> DDFData a -> DDFData a
ddfNextDegree q m ddf =
  let i' = ddfDegree ddf + 1
      xq' = polyModPow (ddfXq ddf) q m
      g = polyGCD (ddfRemain ddf) (xq' - polyX)
```

We will at least be changing `ddfDegree` and `ddfXq`.

```haskell
      nextDDF = ddf{ ddfXq = xq', ddfDegree = i' }
```

If the GCD is just a constant, the polynomial has no irreducible factors
of degree i' and we continue with the same polynomial.

```haskell
  in  if polyDegree g <= 0
      then nextDDF
```

Otherwise, we divide by g and add it as a factor.

```haskell
      else nextDDF{ ddfRemain = ddfRemain ddf `polyQuot` g
                  , ddfFactors = (i', g) : ddfFactors ddf }
```

The DDF function returns a list of factors of the given polynomial,
tagged by the degree of their irreducible factors.

```haskell
polyDDF :: (Eq a, Fractional a) => (Int,Int) -> Poly a -> [(Int,Poly a)]
polyDDF (p,e) f =
  let nextI = ddfNextDegree (fromIntegral p^e) f
```

The iteration starts with the given polynomial,
looking next at polynomials of degree 1.

```haskell
      initData = DDFData{ ddfRemain = f, ddfFactors = []
                        , ddfDegree = 0, ddfXq = polyX }
```

We have completed the iteration when the remaining polynomial
cannot have a part of degree i removed from it.

```haskell
      iterationDone ddf = polyDegree (ddfRemain ddf) < 2 * ddfDegree ddf
      DDFData{ ddfRemain = finalF, ddfFactors = finalFs } =
        head $ dropWhile (not . iterationDone) $ iterate nextI initData
```

Finally, we return our list of factors.
If the remainder is not just a constant, then it too is a factor.

```haskell
  in  case polyDegree finalF of
        0 -> finalFs
        d -> (d, finalF) : finalFs
```

### Equal-degree factorization

We use the fact that GF(( 2^128 )^d) is isomorphic to
the polynomials over GF(2^128) modulo an irreducible polynomial of degree d
(like all of the factors of d).
The multiplicative order of GF(2^(2k)) for any k (i.e. 2^(2k) - 1)
is divisible by 3.
Therefore, taking a random polynomial to the power of (((2^128)^ d - 1) / 3)
takes it (with some probability above 1/3) to 1 modulo any one of f's factors.
Subtracting 1 then takes it to zero, modulo these factors,
so its GCD with f will be exactly the product of those factors.
Hopefully some factors will be in the GCD and some will not!
Then we can divide out to split f into two pieces, and repeat the process
to recursively find every individual factor.

We need to be able to generate random polynomials;
this in turn will require a method for generating random coefficients.

```haskell
randomPolyR :: R.MonadRandom m => m a -> Int -> m (Poly a)
randomPolyR genCoeff deg = do
  coeffs <- replicateM (deg+1) genCoeff
  pure $ Poly . IM.fromAscList . zip [0..deg] $ coeffs
```

Now the equal-degree factorization.
It factors a polynomial, all of whose irreducible factors
are of the same degree d, into those irreducible factors.
(Right now it's only implemented over GF(2^(2k))).

```haskell
polyEDF :: (Fractional a, Eq a, R.MonadRandom m)
        => m a -> (Int,Int) -> Poly a -> Int -> m [Poly a]
polyEDF genCoeff (p@2,e) f d = go [f]
 where
  q = fromIntegral p^e
  qd3 = (q^d - 1) `div` 3
```

We know the degree of the polynomial and the degree of the factors,
so we know how many factors we're looking for.

```haskell
  n = polyDegree f
  numFs = n `div` d
```

The recursive function `go` generates polynomials
and tries to split the factors.

```haskell
  go fs
```

If we have all of the factors already, we just return them.

```haskell
    | length fs == numFs = pure fs
```

Otherwise, we have to try to split again.
We generate a new polynomial of degree (n-1).

```haskell
    | otherwise = do
        h <- randomPolyR genCoeff (n - 1)
```

We want a polynomial with common factors with f.
h might already be such a polynomial;
otherwise, we take its `(q^d - 1)/3` power and try that.

```haskell
        let g = polyGCD h f
            g' = if polyDegree g > 0
                 then g
                 else polyModPow h qd3 f - 1
```

We try splitting each of the factors with the polynomial,
then recurse.

```haskell
        go $ concatMap (splitFs g') fs
```

The splitting is performed by `splitFs`.

```haskell
  splitFs g u
```

If we're already at an unfactorable polynomial, we're done.

```haskell
    | polyDegree u == d = [u]
```

If the GCD of g and u is neither a constant nor just u itself,
then we can split u into two.

```haskell
    | gcdPoly <- polyGCD g u
    , 0 < polyDegree gcdPoly && polyDegree gcdPoly < polyDegree u =
        [ gcdPoly , u `polyQuot` gcdPoly ]
```

Otherwise, we can't.

```haskell
    | otherwise         = [u]
```

### Putting it all together

The function `polyRoots` combines all of these functions,
returning all of the roots of the given polynomial.

```haskell
polyRoots :: (Fractional a, Eq a, R.MonadRandom m)
          => m a -> (Int,Int) -> Poly a -> m [a]
polyRoots genCoeff (p,e) poly = do
```

We extract the linear factors of the polynomial.

```haskell
  let linearFactors =
        [ snd (polyMonic f2)
```

First, we get all square-free parts, ignoring multiplicity.

```haskell
        | (_,f1) <- polySFF (p,e) $ snd . polyMonic $ poly
        , polyDegree f1 > 0
```

Next, we get only the linear parts from the distinct-degree factorization.

```haskell
        , (1,f2) <- polyDDF (p,e) $ snd . polyMonic $ f1 ]
```

Now we use `polyEDF` on every product of linear factors
to extract the individual linear polynomials.

```haskell
  lfs <- forM linearFactors $ \f -> polyEDF genCoeff (p,e) f 1
```

Finally, we solve each linear factor (if possible).
This just involves extracting the coefficients and dividing.

```haskell
  let solve p = (/) <$> polyCoeff p 0 <*> (negate <$> polyCoeff p 1)
  pure $ mapMaybe solve $ concat lfs
```

`gcmPoly` generates the polynomial corresponding to the given signature,
with the hash as the constant term.
The generation of the polynomial is effectively identical to `gcmHash`,
but multiplies at each term by the `polyX` polynomial instead of h.

```haskell
gcmPoly :: (HasBytes ad, HasBytes nonce) => GCMMAC nonce ad -> Poly GF
gcmPoly MAC{ macMessage = (iv,ad,ct), macHash = hash } =
  let hashText = B.concat [ zeroPad ad, zeroPad ct
                          , bigEndian 8 (8 * numBytes ad)
                          , bigEndian 8 (8 * numBytes ct) ]
      blocks = map (toPoly . convBytes) $ chunksOf blockSize hashText
      polyTerm g b = (g + b) * polyX
  in  toPoly hash + foldl' polyTerm 0 blocks
 where
  blockSize = 16

  zeroPad bs =
    let sz = numBytes bs
        pad = blockSize * (sz `cdiv` blockSize) - sz
    in  toBytes bs <> B.replicate pad 0
```

`findGCMKey` uses `polyRoots` to compute the GCM authentication key h
and secret s given two GCM-MACs which are generated using the same nonce.
It requires a validation oracle to tell if it's found the right values.

```haskell
findGCMKey :: (R.MonadRandom m, HasBytes nonce, HasBytes ad)
           => GCMMAC nonce ad -> GCMMAC nonce ad
           -> (GCMMAC nonce ad -> Bool)
           -> m (GF,GF)
findGCMKey sig1 sig2 oracle = do
```

Since the GCM hash is a polynomial in h with constant term s,
if we have two hashes with the same s
we can just add the corresponding polynomials and the constant terms
will cancel out; since we know all of the remaining coefficients
(they are the blocks of the messages)
we can formulate the sum polynomial and factor it to find h.

We use `polyRoots` to find the roots of the polynomial
`(gcmPoly sig1 + gcmPoly sig2)` in the field GF(2^128).

```haskell
  roots <- polyRoots randomGF (2,128) (gcmPoly sig1 + gcmPoly sig2)
```

We may get several roots, but we can check them by
attempting a forgery using the candidate key and nonce.

```haskell
  pure $ head [ (h,s) | h <- roots
                      , let s = getS sig1 h
                      , testKey h s ]
```

The nonce corresponding to a given candidate key h
is just the polynomial corresponding to a signature,
evaluated at h.

```haskell
 where
  getS sig h = polyEval (gcmPoly sig) h
```

We can test a candidate key by attempting a forgery.

```haskell
  testKey h s =
    let MAC{ macMessage = (nonce,_,_) } = sig1
        evilAD = convBytes "to: alice from: bob, really!"
        evilCT = convBytes "an evil ciphertext"
    in  oracle $ forgeGCM h s (nonce, evilAD, evilCT)
```

Finally, given an authentication key h and a nonce-derived secret s,
`forgeGCM` creates a forged GCM-MAC.
It's essentially the function `mkGCMMACHash`,
using the given h and s instead of deriving them from the encryption key.

```haskell
forgeGCM :: (HasBytes nonce, HasBytes ad)
         => GF -> GF -> (nonce,ad,Bytes) -> GCMMAC nonce ad
forgeGCM h s (nonce,ad,ct) = MAC{ macMessage = (nonce,ad,ct), macHash = hash }
 where
  blockSize = 16

  zeroPad bs =
    let sz = numBytes bs
        pad = blockSize * (sz `cdiv` blockSize) - sz
    in  toBytes bs <> B.replicate pad 0

  hashText = B.concat [ zeroPad ad, zeroPad ct
                      , bigEndian 8 (8 * numBytes ad)
                      , bigEndian 8 (8 * numBytes ct) ]
  hash = gcmHash h hashText + s
```
