# Solution to Challenge 11

This module contains the random ECB/CBC cipher generator
and the ECB/CBC detection oracle
required to solve Challenge 11.

```haskell
module Challenge11
  (
    randomAESCipher, ecbDetector
  ) where

import Util ( countRepeats )
import Bytes ( chunksOf )
import Random ( randomBytes, randomBytesR )
import AES ( encryptECB, encryptCBC )
import BlockTools ( BlockEncryptOracle, fillerText )
import Padding.PKCS7 ( padPKCS7 )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
import qualified Control.Monad.Random as R
```

We have a function which creates a random AES cipher, either ECB or CBC.
The function returns both the encoding function
and a boolean determining the actual variant used,
so we can check if we guess correctly.

```haskell
randomAESCipher :: R.MonadRandom m => m (Bool, BlockEncryptOracle)
randomAESCipher = do
  isECB <- R.getRandom
  key <- randomBytes 16
  iv  <- randomBytes 16
  prepad  <- randomBytesR (5,10)
  postpad <- randomBytesR (5,10)
  let padded text = padPKCS7 16 $ B.concat [prepad,text,postpad]
  pure ( isECB , case isECB of
                   True  -> fromJust . encryptECB key    . padded
                   False -> fromJust . encryptCBC key iv . padded )
```

Detecting this is fortunately simple.
Recalling that ECB will encode identical blocks to identical blocks,
we just encrypt a long text composed of a repeated block.

```haskell
ecbDetector :: Int -> BlockEncryptOracle -> Bool
ecbDetector blockSize oracle =
  let numReps = 10
      plaintext = B.concat $ replicate numReps $ fillerText blockSize
      encrypted = oracle plaintext
```

If the text has `numReps` repetitions of this block,
we expect the encrypted text will contain at least `numReps-1` of them
(given the frame shift induced by the prepadding).
Anything less than `numReps-2` repeated blocks, therefore,
means we're seeing a CBC encryption.

```haskell
  in  countRepeats (chunksOf blockSize encrypted) >= (numReps - 2)
```
