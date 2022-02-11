# Solution to Challenge 27

```haskell
module Challenge27
  (
    weakCBC
  , WeakCBCResult(..)
  , validateWeakCBC
  , breakWeakCBC
  ) where

import Bytes ( HasBytes(..), Bytes, xorb, chunksOf )
import AES ( encryptCBC, decryptCBC )
import Padding.PKCS7 ( padPKCS7, validatePKCS7 )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
```

We create a "weak CBC", which uses the same value for key and IV.

```haskell
weakCBC :: (HasBytes key, HasBytes text) => key -> text -> Bytes
weakCBC key = fromJust . encryptCBC key key . padPKCS7 16 . toBytes
```

Our validator checks for high-ASCII values.
It returns the decrypted plaintext if it's found to be invalid.
This leads to three possible outputs:
validated, decryption unsuccessful, or plaintext invalid.

```haskell
data WeakCBCResult = OK | DecryptFailure | Invalid Bytes
  deriving Show
```

```haskell
validateWeakCBC :: HasBytes key => key -> Bytes -> WeakCBCResult
validateWeakCBC key text = case decryptCBC key key text >>= validatePKCS7 of
  Nothing -> DecryptFailure
  Just plaintext | B.all (<=127) plaintext -> OK
                 | otherwise               -> Invalid plaintext
```

Now we can break the system and find our key.

```haskell
breakWeakCBC :: (Bytes -> WeakCBCResult) -> Bytes -> Maybe Bytes
breakWeakCBC oracle ciphertext =
```

The first three blocks of a message would normally be decrypted as

    P1 = D(C1) ^ KEY, P2 = D(C2) ^ C1, P3 = D(C3) ^ C2.

We remove the first three blocks from the ciphertext;
we won't need to keep the second or third.

```haskell
  let c1:_:_:cs = chunksOf 16 ciphertext
```

We replace the first three blocks of the ciphertext with the three blocks
`(C1 | 0 | C1)`.

```haskell
      evilCT = B.concat (c1 : B.replicate 16 0 : c1 : cs)
```

The decryption now becomes

    P1' = D(C1) ^ KEY, P2' = D(0) ^ C1, P3' = D(C1) ^ 0.

and so,

    P1' ^ P3' = (D(C1) ^ KEY) ^ (D(C1) ^ 0) = KEY.

```haskell
  in  case oracle evilCT of
        Invalid plaintext ->
          let p1':_:p3':_ = chunksOf 16 plaintext
          in  Just (p1' `xorb` p3')
        _ -> Nothing
```

The `Nothing` will be returned only if none of the bytes of the decrypted text
has a high bit set. The first block decrypts to the original text,
but the next three are changed to

    P2' = D(0) ^ C1, P3' = D(C1) ^ 0, P4' = D(C4) ^ C1

which might as well be random blocks.
If the new values are entirely uncorrelated,
the probability of no high bits is something like
one in 200 trillion. Needless to say, I didn't test this case.
