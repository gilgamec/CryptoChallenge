# Solution to Challenge 54

```haskell
module Challenge54
  (
    buildCollisionTree
  , finalHash, glueMessage
  ) where

import Bytes ( HasBytes(..), Bytes, chunksOf )
import Hash.Collision ( findCollision2 )
import Padding.Hash ( sha1Padding )
import Util ( splitPairs )

import Control.Monad ( replicateM )
import Data.List ( foldl' )
import Data.Maybe ( maybeToList )

import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
```

For this attack, we're going to create a binary tree of hash collisions.
At each node, we have a hash state,
while each subtree of an internal node is labeled with a glue block
which hashes, with the initial state of that node,
to the parent node's hash state.

```haskell
data CollisionTree = CTree
  { ctHash :: Bytes
  , ctChildren :: Maybe ((Bytes,CollisionTree), (Bytes,CollisionTree)) }
  deriving (Eq,Ord,Show)
```

Once again, the glue blocks will consist of lower-case letters.

```haskell
allBlocks :: [Bytes]
allBlocks = map BC.pack $ replicateM 16 ['a'..'z']
```

`collideTrees` finds a collision between two trees
and joins them into one.

```haskell
collideTrees :: (Bytes -> Bytes -> Bytes)
             -> CollisionTree -> CollisionTree -> CollisionTree
collideTrees hashOne c1@CTree{ctHash = h1} c2@CTree{ctHash = h2} =
```

The glue blocks are found with `findCollision2`.

```haskell
  let Just ((b1,b2),h) = findCollision2 (hashOne h1) allBlocks
                                        (hashOne h2) allBlocks
```

We then pair the glue blocks with their respective trees
and make a new node with them as children.

```haskell
  in  CTree{ ctHash = h, ctChildren  = Just ((b1,c1), (b2,c2)) }
```

`buildCollisionTree` is given a list of hash values
and repeatedly calls `collideTrees` to build 
the largest full binary collision tree possible from them.

```haskell
buildCollisionTree :: (Bytes -> Bytes -> Bytes) -> [Bytes] -> CollisionTree
buildCollisionTree hashOne hs =
```

We turn each hash value into a leaf of the tree.

```haskell
  let leaves = map (\hash -> CTree{ ctHash = hash, ctChildren = Nothing }) hs
```

We can split a list of trees into pairs
then collide them into single trees,
resulting in a list half the size of the original.

```haskell
      combine cts = map (uncurry $ collideTrees hashOne) (splitPairs cts)
```

We repeat this combining step until we are left with a singleton list.
This single tree is then returned.

```haskell
  in  head $ head . dropWhile ((>1) . length) $ iterate combine leaves
```

The final hash value of our "prediction"
will hash the final state of our collision tree
one more time through the padding blocks.
In order to build the padding, we'll need to predetermine how long
(in blocks) our initial message will be.

```haskell
finalHash :: (Bytes -> Bytes -> Bytes) -> CollisionTree -> Int -> Bytes
finalHash hashOne ct msgLen =
```

The length of the final message will be
the length of the initial message
plus one glue block
plus the depth of the tree.

```haskell
  let len = msgLen + 1 + treeDepth ct

      treeDepth CTree{ ctChildren = Nothing } = 0
      treeDepth CTree{ ctChildren = Just ((_,t),_) } = 1 + treeDepth t
```

The padding blocks will then be

```haskell
      paddingBlocks = chunksOf 16 $ sha1Padding (len * 16)
```

with a hash of

```haskell
  in  foldl' hashOne (ctHash ct) paddingBlocks
```

In order to use the collision tree in forgery,
we will need to generate a map from a leaf's hash
to the blocks stored on its path to the root.

```haskell
leafMap :: CollisionTree -> M.Map Bytes [Bytes]
leafMap = fmap ($[]) . go
 where
```

Since we start at the root of the tree,
we have to build the blocks in reverse order;
they are thus built with difference lists.
The recursive function `go` builds all pairs
`(hash,blocks)` for the paths in the tree.
For a leaf, the hash is reached before any blocks are processed.

```haskell
  go CTree{ ctHash = hash, ctChildren = Nothing } = M.singleton hash id
```

For an internal node, we concatenate all paths in both children,
adding the corresponding glue block at the back.

```haskell
  go CTree{ ctChildren = Just ((b1,c1),(b2,c2)) } =
    fmap (. (b1:)) (go c1) <> fmap (. (b2:)) (go c2)
```

Once we know what will be in our message,
we can turn it into a message hashing to the final value
by taking the source message and generating a glue block
to one of the leaf hashes in the collision tree.

```haskell
glueMessage :: HasBytes text
            => (Bytes -> Bytes -> Bytes) -> Bytes -> CollisionTree
            -> text -> Bytes
glueMessage hashOne iv tree message =
```

We split the message into blocks and compute its hash.

```haskell
  let msgBlocks = chunksOf 16 message
      msgHash = foldl' hashOne iv msgBlocks
```

The function `hashLookup` hashes a glue block
then looks up the hash in the leaf-path map.

```haskell
      hashMap = leafMap tree
      hashLookup glue = M.lookup (hashOne msgHash glue) hashMap
```

The glue block is then the first for which
`hashLookup` returns a path rather than `Nothing`.

```haskell
      (glueBlock,treePath) =
        head [ (glue,path)
             | glue <- allBlocks
             , path <- maybeToList (hashLookup glue) ]
```

The final message is then

```haskell
  in  B.concat $ msgBlocks ++ [ glueBlock ] ++ treePath
```
