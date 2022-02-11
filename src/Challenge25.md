# Solution to Challenge 25

```haskell
module Challenge25
  (
    editCTR
  , breakEditCTR
  ) where

import Bytes ( HasBytes(..), Bytes, xorb, chunksOf )
import AES ( aesCTRBlocks )
import Util ( cdiv )

import Data.List ( splitAt, zipWith4 )
import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
```

The function `editCTR` applies an edit 'through' CTR encryption.

```haskell
editCTR :: (HasBytes key, HasBytes nonce, HasBytes newtext)
        => key -> nonce -> Int -> newtext -> Bytes -> Bytes
editCTR key nonce offset newtext bs =
```

The first step is to acquire the blocks which are affected by the edit.

```haskell
  let firstBlock = offset `div` blockSize
      lastBlock = (offset + numBytes newtext) `cdiv` blockSize
      numBlocks = lastBlock - firstBlock
      (preC,crest) = B.splitAt (firstBlock * blockSize) bs
      (cText,postC) = B.splitAt (numBlocks * blockSize) crest
      cBlocks = chunksOf blockSize cText
```

The CTR blocks are taken from the keystream.
Note that we only need to compute the blocks for the changed text.

```haskell
      kBlocks = take numBlocks $ fromJust $ aesCTRBlocks key nonce firstBlock
```

The new text is split into blocks matching the edit blocks.
This means that we have to offset the piece of text for the first block
before using `chunksOf`.

```haskell
      firstOffset = offset - (firstBlock * blockSize)
      (nBlock1, nrest) = B.splitAt (blockSize - firstOffset) (toBytes newtext)
      nBlocks = chunksOf blockSize nrest
```

The edit is applied to each block individually.
The function takes the ciphertext block, keystreak block,
new plaintext, and offset (which is zero for all blocks but the first).

```haskell
      eBlocks = zipWith4 editBlock cBlocks
                                   kBlocks
                                   (nBlock1 : nBlocks)
                                   (firstOffset : repeat 0)
```

Finally, we paste the edited blocks back together with the unaffected text.

```haskell
  in  preC <> B.concat eBlocks <> postC
 where
  blockSize = 16
```

Editing a single block just cuts the plaintext block at the offset
then splices the new text in place of the old.

```haskell
  editBlock ctext keys ntext off =
    let ptext = ctext `xorb` keys
        (pre,rest) = B.splitAt off ptext
        post = B.drop (numBytes ntext) rest
    in  (pre <> toBytes newtext <> post) `xorb` keys
```

Breaking this system is tremendously simple,
since we can set the plaintext to anything we want and get the new ciphertext,
which is the plaintext XORed with the secret keystream.
But if our chosen text is entirely zeros,
then the ciphertext will just be the keystream itself!

```haskell
breakEditCTR :: (Int -> Bytes -> Bytes -> Bytes) -> Bytes -> Bytes
breakEditCTR edit ctext =
  let keystream = edit 0 (B.replicate (numBytes ctext) 0) ctext
  in  ctext `xorb` keystream
```
