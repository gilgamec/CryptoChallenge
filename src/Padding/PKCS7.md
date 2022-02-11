# PKCS7 padding

This module contains functions to add and validate
[PKCS#7 padding](https://tools.ietf.org/html/rfc5652).

```haskell
module Padding.PKCS7
  (
    padPKCS7
  , validatePKCS7
  ) where

import Bytes ( HasBytes(..), Bytes, splitEnd )

import qualified Data.ByteString as B
```

PKCS#7 padding appends enough bytes to the text
to fill it out to a whole number of blocks,
with all of the appended bytes being the number of bytes appended.
Thus, if we have to add one byte, that byte is 0x01;
if we have to add six, those six bytes are all 0x06.
If the text is already the length of a whole number of blocks,
we add an entire extra block of padding.

```haskell
padPKCS7 :: HasBytes text => Int -> text -> Bytes
padPKCS7 blockSize text =
  let bytes = toBytes text
      paddingLength = blockSize - (B.length bytes `rem` blockSize)
      padding = B.replicate paddingLength (toEnum paddingLength)
  in  bytes <> padding
```

We can validate PKCS#7 by checking that the last `n` characters are just `n`.

```haskell
validatePKCS7 :: Bytes -> Maybe Bytes
validatePKCS7 bs
```

No valid padding ends in a zero byte.

```haskell
  | lastVal == 0 = Nothing
```

The entire message must be at least as long as the final byte.

```haskell
  | numBytes bs < fromIntegral lastVal = Nothing
```

The last `lastVal` bytes must be equal to `lastVal`.

```haskell
  | B.any (/= lastVal) lastBunch = Nothing
```

Otherwise, we can just return the message stripped of the final `lastVal` bytes.

```haskell
  | otherwise = Just message
 where
  lastVal = B.last bs
  (message,lastBunch) = splitEnd (fromIntegral lastVal) bs
```
