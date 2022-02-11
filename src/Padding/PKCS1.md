# PKCS1 padding

This module contains functions to add and validate
[PKCS#1 padding](https://tools.ietf.org/html/rfc8017).

```haskell
module Padding.PKCS1
  (
    padPKCS1, padPKCS1'
  , validatePKCS1
  ) where

import Bytes ( HasBytes(..), Bytes, Byte )

import Control.Monad ( guard, replicateM )

import qualified Data.ByteString as B
import qualified Control.Monad.Random as R
import qualified Data.Attoparsec.ByteString as P
```

PKCS#1v1.5 padding pads a message to the left with the string

    00 | PT | PS | 00

where PT is the padding type (1 or 2) and PS is the padding
(0xff for PT=1, random nonzero bytes for PT=2).
The total length of the message + padding
should be the (RSA) block size.

Since we may need random bytes,
the padding has to take place in a `MonadRandom`.

```haskell
padPKCS1 :: (HasBytes text, R.MonadRandom m)
         => Byte -> Int -> text -> m (Maybe Bytes)
padPKCS1 padType blockSize text = do
```

From the block size and the number of bytes in the text,
we can figure out the length of the padding.
The padding length must be at least 8 bytes;
we'll be checking later that everything fits in the block size.

```haskell
  let paddingLength = max 8 $ blockSize - 3 - numBytes text
```

The padding is either (type 1) all 0xff, or (type 2) random nonzero bytes.

```haskell
  padding <- case padType of
    1 -> pure $ B.replicate paddingLength 0xff
    2 -> B.pack <$> replicateM paddingLength nonzeroByte
```

We can then put the block together
and check its length.

```haskell
  let block = B.concat [ B.pack [0, padType], padding
                       , B.singleton 0, toBytes text ]
  pure $ if numBytes block > blockSize
           then Nothing
           else Just block
```

We use `getRandomR` to generate a random nonzero byte.

```haskell
nonzeroByte :: R.MonadRandom m => m Byte
nonzeroByte = R.getRandomR (1, 0xff)
```

Padding type 1 needs no randomness,
so we have a non-monadic padding operation for it.

```haskell
padPKCS1' :: HasBytes text => Int -> text -> Maybe Bytes
padPKCS1' blockSize text =
  let paddingLength = max 8 $ blockSize - 3 - numBytes text
      block = B.concat [ B.pack [0,1], B.replicate paddingLength 0xff
                       , B.singleton 0, toBytes text ]
  in  if numBytes block > blockSize  
        then Nothing
        else Just block
```

### Validating PKCS1 padding.

We validate the PKCS1 padding using a parser from the
[attoparsec](https://hackage.haskell.org/package/attoparsec) package.
This might be overkill, but it certainly works.

```haskell
validatePKCS1 :: HasBytes block => Int -> block -> Maybe Bytes
validatePKCS1 blockSize input =
```

We ensure that the block size matches before trying to parse.
This is important because the `Integer`-to-`Bytes` conversion
will drop the leading zero from the number.

```haskell
  let block = B.replicate (blockSize - numBytes input) 0 <> toBytes input
```

We run the parser on the block and return `Nothing` if it fails.

```haskell
  in  either (const Nothing) Just $ P.parseOnly paddingParser block
```

The parser itself looks for the padding pattern

    00 | PT | PS | 00

where PT is 0 or 1, and determines what the PS bytes are.
The remaining bytes are then the message.

```haskell
 where
  paddingParser = do
    P.word8 0
    padType <- P.anyWord8
    guard $ padType == 1 || padType == 2
    pad <- case padType of
             1 -> P.takeWhile1 (==0xff)
             2 -> P.takeWhile1 (>0)
    guard $ numBytes pad >= 8
    P.word8 0
    msg <- P.takeByteString
    guard $ numBytes pad + numBytes msg + 3 == blockSize
    pure msg
```
