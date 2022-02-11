# Solution to Challenge 14

```haskell
module Challenge14
  (
    decryptInfixECB
  ) where

import Bytes ( Bytes )
import BlockTools ( BlockEncryptOracle, fillerText
                  , findBlockSize, findPrefixLength )

import Challenge11 ( ecbDetector )
import Challenge12 ( decryptPrefixECB )

import qualified Data.ByteString as B
```

We've already built everything we need for this Challenge.
The idea is to turn the infix oracle into a prefix oracle
and solve it with `decryptPrefixECB`.

```haskell
decryptInfixECB :: BlockEncryptOracle -> Bytes
decryptInfixECB oracle
  | not (ecbDetector blockSize oracle) =
    error "decryptInfixECB: Must be run on an ECB-encrypting oracle."
  | otherwise = decryptPrefixECB (dropPrefix . oracle . addFiller)
 where
  blockSize = findBlockSize fillerText oracle
```

To do this, we figure out the length of the prefix (with `findPrefixLength`);

``` haskell
  prefixSize = findPrefixLength blockSize fillerText oracle
```

prepend enough bytes to to the input text to fill out the prefix blocks
to a full block size;

``` haskell
  prefixFillerLength = (-prefixSize) `mod` blockSize
  addFiller txt = fillerText prefixFillerLength <> txt
```

then cut off those unchanging prefix blocks from the ciphertext returned.

``` haskell
  dropPrefix = B.drop (prefixSize + prefixFillerLength)
```
