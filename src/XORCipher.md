# Mono- and polyalphabetic XOR ciphers

This module contains functions which perform and break
monoalphabetic XOR ciphers.

```haskell
module XORCipher
  (
    monoXOR, breakMonoXOR
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
