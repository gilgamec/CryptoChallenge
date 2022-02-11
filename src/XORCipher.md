# Mono- and polyalphabetic XOR ciphers

This module contains functions which perform and break XOR ciphers.

```haskell
module XORCipher
  (
    monoXOR, breakMonoXOR
  , polyXOR
  ) where

import Bytes ( HasBytes(..), Bytes, Byte, xor )
import Util ( argmax )
import Distribution ( Distribution, countBytes, logLikelihood )

import qualified Data.ByteString as B
```

## Monoalphabetic XOR ciphers

A monoalphabetic cipher simply
XORs every byte in a sequence of bytes by the same value;
it's just a `map` of the `xor` operation.

```haskell
monoXOR :: HasBytes text => Byte -> text -> Bytes
monoXOR x = B.map (xor x) . toBytes
```

To break a monoalphabetic XOR,
we need an expected distribution of the characters in the plaintext.
We then compare every possible decryption (there are only 256)
and choose the one which best matches the expected distribution.

```haskell
breakMonoXOR :: HasBytes text => Distribution -> text -> (Bytes,Byte)
breakMonoXOR dist text =
  let distMatch byte = logLikelihood dist $ countBytes $ monoXOR byte text
      bestByte = argmax distMatch [0..255]
  in  (monoXOR bestByte text, bestByte)
```

## Polyalphabetic XOR ciphers

There are several ways to implement the polyalphabetic XOR:
we could `xorb` the text against many repetitions of the key;
or split the text into key-sized chunks, `xorb` each against the key,
then concatenate them.
Much more efficient than either is using `mapAccumL`,
which doesn't involve allocating any temporary `ByteString`s
and proceeds byte-by-byte along the text,
carrying along the corresponding index into the key.

```haskell
polyXOR :: (HasBytes key, HasBytes text) => key -> text -> Bytes
polyXOR key =
  let key' = toBytes key
      nextIx k = (k + 1) `mod` B.length key'
```

The accumulation function takes the current byte
and the corresponding index into the key,
returning the correct XOR and the next index.

```haskell
      accum k w = (nextIx k, B.index key' k `xor` w)
  in  snd . B.mapAccumL accum 0 . toBytes
```
