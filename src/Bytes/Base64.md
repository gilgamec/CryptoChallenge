# Base64 representation of Bytes

This module describes a
[Base64](https://doi.org/10.17487%2FRFC4648)-encoded
representation of a sequence of bytes.

```haskell
module Bytes.Base64
  (
    Base64, mkBase64
  ) where

import Bytes ( HasBytes(..) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
```

Bytes in Base64 encoding are represented by a `newtype`
over their Base64 representation.

```haskell
newtype Base64 = Base64 { asBase64 :: B.ByteString } deriving (Eq,Ord,Show)  
 ```

Base64 conversions use the
[base64-bytestring](https://hackage.haskell.org/package/base64-bytestring)
package.

```haskell
instance HasBytes Base64 where
  fromBytes = Base64 . B64.encode
```

The Base64 decoding function may return an error.
We assume the encoded string is valid and throw an `error` in this case.
```haskell
  toBytes Base64{asBase64 = b64} = case B64.decode b64 of
    Left err -> error $ "Could not decode base64 "++show b64++": "++show err
    Right bs -> bs
```

We ensure that the Base64 is valid by using the smart constructore `mkBase64`.
It simply converts to `Bytes` and back to ensure the encoding is valid.

```haskell
mkBase64 :: String -> Maybe Base64
mkBase64 str = case B64.decode $ toBytes str of
  Right bs -> Just $ Base64 $ B64.encode bs
  Left err -> Nothing
```
