# AES encryption and decryption

This module contains functions which encrypt and decrypt
using the AES block cipher.

```haskell
module AES
  (
    encryptECB, decryptECB
  ) where

import Bytes ( HasBytes(..), Bytes )
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
