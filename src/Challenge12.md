# Solution to Challenge 12

This module contains the function `decryptPrefixECB`,
which solves Challenge 12.

```haskell
module Challenge12
  (
    decryptPrefixECB
  ) where

import Bytes ( HasBytes(..), Bytes, Byte, chunksOf, takeEnd )
import BlockTools ( BlockEncryptOracle, fillerText
                  , findBlockSize, findAddedLength )

import Challenge11 ( ecbDetector )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
```

Challenge 12 involves finding some secret text
which is appended to chosen text then encrypted by an oracle.
This text can be decrypted byte-by-byte, from the beginning.

Suppose we know some prefix KKKKKKKKK of the secret.
We can find the next character T by:

1. Encrypting a filler text of the correct size that the first unknown byte,
   the target byte, falls at the end of a block.
   
    FFFFFFKKKKKKKKKT UUUUUUUU....

   The block with the target byte is our query block.

2. Since we know the 15 bytes that fall before the target byte (either in
   the part of the secret that we know, or in the filler text), we know that
   the plaintext of the query block is one of 256 possibilities

   KKKKKKKKKKKKKKKA, KKKKKKKKKKKKKKKB, ...

   We encrypt each of these as *chosen texts*;
   they make up complete blocks and one of them will encrypt to
   the same as the query block, telling us what the target byte is.

The function `nextByte` performs this operation.

```haskell
nextByte :: Int -> BlockEncryptOracle -> Bytes -> Byte
nextByte blockSize oracle known =
```

`blockIdx` and `offset` are the position in the secret
of the current unknown byte.

```haskell
  let (blockIdx, offset) = numBytes known `quotRem` blockSize
```  

We have to add enough filler to shift `offset` to `blockSize - 1`.

```haskell
      shiftPrefix = fillerText (blockSize - 1 - offset)
```

The query block is the `blockIdx` block of this ciphertext.

```haskell
      queryBlock = (chunksOf blockSize $ oracle shiftPrefix) !! blockIdx
```

Now we build the table of blocks of the form FFFFFFKKKKKKKKKx.
The prefix is the last `blockSize-1` bytes
from `shiftPrefix <> known`.

```haskell
      tablePrefix = takeEnd (blockSize - 1) (shiftPrefix <> known)
```

We encrypt with this as chosen text, over all possible final bytes,
and take the first block.

```haskell
      table = map (B.take blockSize . oracle . B.snoc tablePrefix) [0..255]
```

Exactly one of these first blocks will match our query block.

```haskell
  in  fromJust $ lookup queryBlock $ zip table [0..255]
```

Now we can decrypt a prefix ECB.

```haskell
decryptPrefixECB :: BlockEncryptOracle -> Bytes
decryptPrefixECB oracle
```

This, of course, won't work if the oracle isn't perform ECB encryption.
Fortunately, we can test for this!

```haskell
  | not (ecbDetector blockSize oracle) =
      error "decryptPrefixECB: Must be run on an ECB-encrypting oracle."
```

If we have an ECB oracle, we can iterate `nextByte` to
get the secret, one byte at a time.

```haskell
  | otherwise =
      let expand known = known `B.snoc` nextByte blockSize oracle known
```

We use `findAddedLength` to find the secret's length,
and find exactly that many bytes.

```haskell
          secretSize = findAddedLength fillerText oracle
      in  iterate expand B.empty !! secretSize
 where
  blockSize = findBlockSize fillerText oracle
```
