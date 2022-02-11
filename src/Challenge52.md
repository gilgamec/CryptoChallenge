# Solution to Challenge 52

```haskell
module Challenge52
  (
    mdChained
  , chainHashCollision
  ) where

import Bytes ( HasBytes(..), Bytes )
import Hash.Collision ( multiBlockCollision )
import Hash.MerkleDamgard ( mdHash, mdHashOne, mdIV )

import Control.Monad ( replicateM )
import Data.List ( foldl', unfoldr )
import Data.Maybe ( fromJust )

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
```

We specialize the general `multiBlockCollision` to work with
MD hashes of a given strength.

```haskell
multiBlockMDCollision :: Int -> Bytes -> [(Bytes,Bytes)]
multiBlockMDCollision hashSize iv =
  multiBlockCollision (mdHashOne hashSize) iv candidates
 where
```

The candidate blocks for every position
are just all possible blocks in our alphabet,
the lower-case letters. The list can be simply generated
by running `replicateM` in the list monad.

```haskell
  candidates = repeat $ map BC.pack $ replicateM 16 ['a'..'z']
```

Allegedly, chaining hash functions strengthens them.
A chained hash is built by combining two MD hash functions
of different strengths:

```haskell
mdChained :: HasBytes text => Int -> Int -> text -> Bytes
mdChained h1 h2 text = mdHash h1 text <> mdHash h2 text
```

But it's easy to find collisions here too.

```haskell
chainHashCollision :: Int -> Int -> (Bytes,Bytes)
chainHashCollision hWeak hStrong =
  let hs1 = mdHashOne hStrong
```

We first generate a bunch of collisions for the weaker hash function:

```haskell
      weakCollisions = multiBlockMDCollision hWeak (mdIV hWeak)
```

Every element of `weakCollisions` has 2 colliding choices for each block,
so from an n-element sequence we can create 2^n possible n-block sequences,
all of which collide after every block.
Now we're going to move along the list of colliding blocks
and compute the strong hashes for every combination at every point.
We will collect these in a `Map` from hashes to sequences;
or, because it makes finding collisions simpler, from hashes to
`Either Collision Blocks`.

```haskell
      initMap = M.singleton (mdIV hStrong) (Right B.empty)
```

For each successive block, we want to augment the map of hashes,
adding both blocks to every sequence.
This is done in a left-scan, which creates a sequence of maps.

```haskell
      hashMaps = scanl nextBlock initMap weakCollisions
```

We add the next collided block to our map with the `nextBlock` function.

```haskell
      nextBlock m (b1,b2) =
```

From each element `(hash, Right sequence)` in the map we create two new elements,
each representing the hash and sequence obtained by
adding each of the colliding input blocks.
The new hash is the hash function applied to the block,
with the old hash as the initial state;
the new sequence just appends the new block.

```haskell
        let newAssocs = [ (hs1 iv b, Right $ seq <> b)
                        | (iv, Right seq) <- M.assocs m
                        , b <- [b1,b2] ]
```

When we combine these elements back into a map,
we look out for hash collisions, which will be key collisions in the map.
`Map`'s `fromListWith` function creates a map from a bunch of elements,
calling the given function on key collisions.
Our function records collisions as `Left` values.

```haskell
            collide (Right w1) (Right w2) = Left (w1,w2)
            collide (Left ws) _ = Left ws
            collide _ (Left ws) = Left ws
       in   M.fromListWith collide newAssocs
```  

`Left` values, i.e. collisions, can be extracted from a map
by calling `sequence` on it.
We can thus easily find the first collision.

```haskell
  in  head [ (seq1,seq2) | Left (seq1,seq2) <- map sequence hashMaps ]
```
