# Hexadecimal representation of Bytes

This module describes a hex-encoded representation of a sequence of bytes.

```haskell
module Bytes.Hex
  (
    Hex, mkHex
  , showHex
  ) where

import Bytes ( HasBytes(..), convBytes )

import Data.Char ( isHexDigit )
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
```

Bytes in hexadecimal encoding are represented by a `newtype`
over their hex representation.

```haskell
newtype Hex = Hex { asHex :: B.ByteString } deriving (Eq,Ord,Show)
```

Hex conversions are pretty simple, using the
[base16-bytestring](https://hackage.haskell.org/package/base16-bytestring)
package.
The encoded string is a valid hex encoding.

```haskell
instance HasBytes Hex where
  fromBytes = Hex . B16.encode
```

The decoding function may return an error.
We assume the encoded string is valid and throw an `error` in this case.

```haskell
  toBytes Hex{asHex = hex} = case B16.decode hex of
    Left err -> error $ "Could not decode hex "++show hex++": "++show err
    Right bs -> bs
```

We ensure that every `Hex` is valid using the smart constructor `mkHex`.
It simply tests to see if the decoding to `Bytes` is valid.

```haskell
mkHex :: String -> Maybe Hex
mkHex str = case B16.decode hex of
  Right{} -> Just $ Hex hex
  Left{} -> Nothing
 where
  hex = toBytes str
```

---

We can use the `Hex` conversion to output
the hexadecimal representation of any byte sequence.
This is often more useful than showing the `Bytes` themselves
(as characters not printable in ASCII show up as escape sequences).

```haskell
showHex :: HasBytes a => a -> String
showHex = BC.unpack . asHex . convBytes
```
