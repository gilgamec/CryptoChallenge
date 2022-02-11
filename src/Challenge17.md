# Solution to Challenge 17

```haskell
module Challenge17
  (
    checkCBCPadding
  , CBCToken, CBCPaddingOracle
  , breakCBCPadding
  ) where

import Bytes ( HasBytes(..), Bytes, Byte, xor, xorb, chunksOf )
import AES ( decryptCBC )
import Padding.PKCS7 ( validatePKCS7 )
import Util ( seqPairs )

import Data.Maybe ( isJust )

import qualified Data.ByteString as B
```

First, we need a function that checks the padding of
a CBC-encoded string, given the key and IV.
It only reports whether the padding is valid, nothing else.

```haskell
checkCBCPadding :: (HasBytes key, HasBytes iv) => key -> iv -> Bytes -> Bool
checkCBCPadding key iv msg = isJust $ decryptCBC key iv msg >>= validatePKCS7
```

A "token" for this case is a pair of the IV and the ciphertext;
we'll take both as `Bytes`.
The oracle takes a token and reports whether the padding is OK.

```haskell
type CBCToken = (Bytes,Bytes)
type CBCPaddingOracle = CBCToken -> Bool
```

---

We can use a padding oracle to decrypt a CBC-encrypted text.
The basic idea is this:

1. Remember that we can XOR the ciphertext of a block
   with something to apply that XOR to the plaintext
   of the next block.
   We use this idea to create 256 messages,
   each with the final byte of the first block
   XORed with a different byte.
   Exactly one of the corresponding plaintexts
   (say, XORed by `k`) will have its last byte set to `0x01`.
   But this is a valid padding!
   Thus, the padding oracle will report that test message `k`
   has valid padding, and we then know that the actual
   plaintext byte is `k xor 1`!
2. Now that we know the last byte, we can intentionally set it to 2
   and test 256 messages with a different next-to-last byte.
   The correct padding has this byte set to 2, so we
   know the next-to-last byte of the plaintext.
3. We can then repeat by setting the last two bytes to 3
   to find the antepenultimate byte, and so on.

As the padding oracle doesn't care how long the message is,
we only have to give it two blocks: it treats the first as IV
and the second as the "message" whose padding must be checked.
Since the IV functions in CBC as effectively a ciphertext block,
we can perform this operation with any two sequential blocks.
We thus have a function to decrypt a single block.

```haskell
decryptBlock :: CBCPaddingOracle -> (Bytes,Bytes) -> Bytes
decryptBlock isValid (iv,bs) =
```

Just as in Challenge 12, we chain together repeated invocations
of a single function `nextByte` that decrypts a single byte of the block.

```haskell
  let expand known = nextByte isValid (iv,bs) known `B.cons` known
```

There are the same number of bytes to find in the plaintext
as in the ciphertext.

```haskell
  in  iterate expand B.empty !! numBytes bs
```

Given this function, our main loop is very simple.

```haskell
breakCBCPadding :: CBCPaddingOracle -> CBCToken -> Bytes
breakCBCPadding isValid (iv,bs) =
```

We break up the input into single sequential block pairs;

```haskell
  let blockSize = numBytes iv
      blockPairs = seqPairs (iv : chunksOf blockSize bs)
```

call `decryptBlock` on each;

```haskell
      plainBlocks = map (decryptBlock isValid) blockPairs
```

and string the decrypted text together.

```haskell
  in  B.concat plainBlocks
```

---

Finally, we come to the workhorse function, `nextByte`,
which decrypts the next byte of the block.

```haskell
nextByte :: CBCPaddingOracle -> (Bytes,Bytes) -> Bytes -> Byte
nextByte isValid (iv,bs) known =
  let blockSize = numBytes bs
```

If we know `n` bytes, then we want to make the last `n+1` bytes padding.

```haskell
      paddingSize = numBytes known + 1
```

Their values all have to equal the padding length.

```haskell
      paddingByte = fromIntegral paddingSize
```

The XOR pattern we use depends on our guess as to what the target byte is.
The first bytes of the block are unaltered,
while we XOR the last `paddingSize` bytes against the padding byte.

```haskell
      xorPattern byte = B.replicate (blockSize - paddingSize) 0 <>
                        B.map (xor paddingByte) (byte `B.cons` known)
```

We apply this XOR pattern for every possible value of the target byte
and keep only those whose padding is judged valid.

```haskell
      validBytes = filter (\byte -> isValid (iv `xorb` xorPattern byte, bs))
                          [0..255]
```

There is a very slight hitch in the method described above.
Suppose the second-last byte of the target block is, say, `0x02`,
or the second- and third-last bytes of the target block are both `0x03`,
or so on.
(This will happen for almost any padded final block.)
There will be *two* byte XORs with valid padding
when finding the last byte of the block:
the one where the last byte is set to 1 (and the padding is of length 1);
and the one where the last byte is set to the same value as the other
final bytes (in which case the padding is of length 2 or 3 or whatever).

To distinguish this case when it occurs
(which can only happen when finding the last byte of the block)
we try a different XOR pattern which flips the bits of the second-last byte;
only one candidate will still have valid padding.

```haskell
      mungePattern byte = B.concat [ B.replicate 14 0
                                   , B.singleton 0xff
                                   , B.singleton (byte `xor` 1) ]
      mungeTest byte = isValid (iv `xorb` mungePattern byte, bs)
```

If `validBytes` has only one element, we know what the next byte is.

```haskell
  in  case validBytes of
        [byte] -> byte
```

However, in the described case `validBytes` may have two elements
and we distinguish them with the `mungeTest`.

```haskell
        [b1,b2] -> if mungeTest b1
                     then b1
                     else b2
```
