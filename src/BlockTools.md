# Utilities for breaking block ciphers

Several of the Challenges involve breaking block ciphers.
This module contains some useful tools.

```haskell
module BlockTools
  (
    BlockEncryptOracle
  , fillerText, FillerGen
  ) where

import Bytes ( Bytes )

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
