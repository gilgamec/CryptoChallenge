# Utilities for breaking block ciphers

Several of the Challenges involve breaking block ciphers.
This module contains some useful tools.

```haskell
module BlockTools
  (
    BlockEncryptOracle
  , fillerText, FillerGen
  , findBlockSize, findAddedLength
  ) where

import Bytes ( HasBytes(..), Bytes )

import Data.List ( group )

import qualified Data.ByteString as B
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
