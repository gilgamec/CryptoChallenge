# AES encryption and decryption

This module contains functions which encrypt and decrypt
using the AES block cipher.

```haskell
module AES
  (
    encryptECB, decryptECB
  , encryptCBC, decryptCBC
  ) where

import Bytes ( HasBytes(..), Bytes, xorb, chunksOf )

import Data.List ( scanl' )

import qualified Data.ByteString as B
```

We use the implementation of AES-128 from the package
[cryptonite](https://hackage.haskell.org/package/cryptonite).

```haskell
import Crypto.Cipher.AES ( AES128 )
import Crypto.Cipher.Types ( Cipher(..), BlockCipher(..) )
import Crypto.Error ( CryptoFailable(..) )
```

cryptonite requires that we initialize the cipher with the key;
this may fail (e.g. if the key is of the wrong length)
and we catch this in a `Maybe`.

```haskell
aesKey :: HasBytes key => key -> Maybe AES128
aesKey key = case cipherInit (toBytes key) of
  CryptoPassed cipher -> Just cipher
  CryptoFailed{}      -> Nothing
```

## ECB mode

Encrypting and decrypting ECB is done by initializing the AES cipher
with the key and calling the appropriate cryptonite function.

```haskell
encryptECB :: (HasBytes key, HasBytes text) => key -> text -> Maybe Bytes
encryptECB key text = ecbEncrypt <$> aesKey key <*> Just (toBytes text)

decryptECB :: (HasBytes key, HasBytes text) => key -> text -> Maybe Bytes
decryptECB key text = ecbDecrypt <$> aesKey key <*> Just (toBytes text)
```

## CBC mode

We manually implement other AES modes based on ECB.

To encrypt in CBC mode, we carry the previous ciphertext
to XOR against the next block.
This is a straightforward `scanl`.

```haskell
encryptCBC :: (HasBytes key, HasBytes iv, HasBytes text)
           => key -> iv -> text -> Maybe Bytes
encryptCBC key iv = case aesKey key of
  Nothing -> const Nothing
  Just cipher -> Just .
```

The scan function `oneCBC`
XORs the previous ciphertext against the current plaintext
and encrypts with ECB.

```haskell
    let oneCBC lastCT thisPT = ecbEncrypt cipher (lastCT `xorb` thisPT)
```

The scan is seeded with the IV, the intial "ciphertext".
The IV is then returned as the first ciphertext block,
so we lop it off with `tail`.

```haskell
        mkCBCBlocks = tail . scanl' oneCBC (toBytes iv)
```

The final encryption splits the plaintext into blocks,
ciphers them with the scan,
then glues the resulting ciphertext back together.

```haskell
    in  B.concat . mkCBCBlocks . chunksOf 16
```

To decrypt CBC, we have to XOR the previous ciphertext
while accumulating the blocks of plaintext.
This is simpler than encryption,
since we have all of the ciphertext from the beginning.

```haskell
decryptCBC :: (HasBytes key, HasBytes iv, HasBytes text)
           => key -> iv -> text -> Maybe Bytes
decryptCBC key iv = case aesKey key of
  Nothing -> const Nothing
  Just cipher -> Just .
```

If we have two successive ciphertext blocks
(or the IV and the first ciphertext block)
then we can decrypt the second block with the function `decOne`.

```haskell
    let decOne lastCT thisCT = lastCT `xorb` ecbDecrypt cipher thisCT
```

Given the blocks of the ciphertext,
we pair each with its predecessor using `decOne`:

```haskell
        decBlocks blocks = zipWith decOne (toBytes iv : blocks) blocks
```

Decrypting the entire text is thus just a matter of splitting into blocks,
decrypting the stream, and gluing back together.

```haskell
    in  B.concat . decBlocks . chunksOf 16
```
