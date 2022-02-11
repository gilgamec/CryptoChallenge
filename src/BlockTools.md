# Utilities for breaking block ciphers

Several of the Challenges involve breaking block ciphers.
This module contains some useful tools.

```haskell
module BlockTools
  (
    BlockEncryptOracle
  , fillerText, FillerGen, alphaFiller
  , findBlockSize, findAddedLength, findPrefixLength
  ) where

import Bytes ( HasBytes(..), Bytes, chunksOf )
import Util ( seqPairs )

import Data.List ( group, transpose )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
```

These tools let us determine things about the block cipher
given an _oracle_,
a function that encrypts or decrypts some text of our choosing,
perhaps processed or added to other text,
with an key that is unknown to us.

```haskell
type BlockEncryptOracle = Bytes -> Bytes
```

We'll be attacking oracles by feeding chosen text to them.
Sometimes only the length of this text matters.
The `fillerText` function generates filler text of the given length;
this'll just be a sequence of the printable ASCII characters,
repeated as needed.

```haskell
fillerText :: FillerGen
fillerText n =
  let (q,r) = n `quotRem` B.length ascii
  in  B.concat (replicate q ascii) <> B.take r ascii
 where
  ascii = B.pack [32..126]
```

The functions in this module take an argument of type `FillerGen`,
so custom filler text can be used in cases where
the content of the filler text also matters.

```haskell
type FillerGen = Int -> Bytes
```

One example of special-case filler is where we have to use letters;
for this, we have `alphaFiller`, which is just the alphabet cycled.

```haskell
alphaFiller :: FillerGen
alphaFiller n = BC.pack $ take n $ cycle ['a'..'z']
```

---

`findBlockSize` does as its name suggests.
Since in any block cipher the size of the ciphertext must be a multiple of
this size, we just have to call the oracle with longer and longer text
until the length of the ciphertext increases.
The difference between the old and new size is one block.

```haskell
findBlockSize :: FillerGen -> BlockEncryptOracle -> Int
findBlockSize filler oracle = let
  textLengths = map (numBytes . oracle . filler) [0..]
  l1:l2:_ = map head $ group textLengths
 in l2 - l1
```

---

`findAddedLength` determines the amount of extra text concatenated
to the input text by the oracle.
We know this to within a single block,
and can find the exact size in pretty much the same way
as we found the block size.
We add more and more text until the block size shifts;
the length we added was just enough to fill out the last block,
and is subtracted from the padded size to get the size of the text itself.

```haskell
findAddedLength :: FillerGen -> BlockEncryptOracle -> Int
findAddedLength filler oracle = let
  textLengths = map (numBytes . oracle . filler) [0..]
  addedLength = length $ head $ group textLengths
 in head textLengths - addedLength
```

---

It's also possible to figure out how the extra data added by the oracle
is distributed between data which comes *before* the user data
and data which comes *after* the user data.
The function `findPrefixLength` finds the length of the prefix data.
The idea is again similar to the other block utilities;
we encrypt with progressively longer chosen text and
see what happens to the encrypted blocks.
Since the prefix doesn't change, its encrypted form also won't change,
so we can identify the prefix by finding blocks which don't change
as the chosen text gets longer.

```haskell
findPrefixLength :: Int -> FillerGen -> BlockEncryptOracle -> Int
findPrefixLength blockSize filler oracle =
```

Each element of `blocks` is a list of the evolution of a single block
as more and more filler text is encrypted.

```haskell
  let blocks = transpose $ map (chunksOf blockSize . oracle . filler) [0..]
```

The blocks which never change contain only prefix data.
We identify these, and the first block which *does* change,
by comparing the first and second instances of each block.

```haskell
      (unchanging, changing:_) = break (\(x1:x2:_) -> x1 /= x2) blocks
```

The changing block starts with the end of the prefix,
but contains the start of the chosen text (and maybe the suffix).
It will stop changing once the chosen text fills it completely;
we can thus tell how much of the block contains the unchanging prefix
by subtracting the number of bytes we added which change the block
from the block size.

```haskell
      numChanging = length $ takeWhile (uncurry (/=)) $ seqPairs changing
  in  blockSize * length unchanging + blockSize - numChanging
```
