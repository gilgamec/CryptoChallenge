# Solution to Challenge 41

```haskell
module Challenge41
  (
    breakUnpadded
  ) where

import PublicKey ( PublicKey(..) )
import PublicKey.RSA ( RSAPublicKey )
import Modulo ( mkMod, modulo, (^%) )
```

The idea here is that users encrypt a secret message
using the server's public key;
then the server will decrypt it and we can capture the plaintext.
But the server will not decrypt a ciphertext twice,
so the attacker can't decrypt another user's message.

Except that we can decrypt (2^e * ciphertext), which decrypts to (2*message).

```haskell
breakUnpadded :: (Integer -> Maybe Integer) -> RSAPublicKey -> Integer
              -> Maybe Integer
breakUnpadded oracle pk c =
  let (e,n) = pkKey pk
      c' = (2 ^% e * mkMod c) `modulo` n
  in  case oracle c' of
        Nothing -> Nothing
        Just p' -> Just $ (mkMod p' / 2) `modulo` n
```
