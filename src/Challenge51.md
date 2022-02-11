# Solution to Challenge 51

```haskell
module Challenge51
  (
    lengthOracleCTR, compressionAttackStream
  , lengthOracleCBC, compressionAttackBlock
  ) where

import Bytes ( HasBytes(..), Bytes )
import Bytes.Base64 ( Base64, mkBase64 )
import AES ( encryptCTR, encryptCBC )
import Padding.PKCS7 ( padPKCS7 )
import BlockTools ( findAddedLength )
import Util ( argmin )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC

import qualified Data.ByteString.Base64 as B64
import qualified Codec.Compression.GZip as Z
```

## Compression oracle

We want to create an oracle which reports the length of a
compressed and encrypted HTTP request.
First, the request format itself. We encode the session ID as Base64.

```haskell
formatRequest :: (HasBytes sid, HasBytes content) => sid -> content -> Bytes
formatRequest sid content = BC.unlines header <> toBytes content where
  header = map toBytes
    [ "POST / HTTP/1.1"
    , "Host: hapless.com"
    , "Cookie: sessionid=" ++ fromBytes (B64.encode $ toBytes sid)
    , "Content-Length: " ++ show (numBytes content)
    ]
```

We compress with the
[zlib](https://hackage.haskell.org/package/zlib) package.
It works on lazy `ByteString`s, so we have to convert to and from.

```haskell
compress :: HasBytes text => text -> Bytes
compress = BL.toStrict . Z.compress . BL.fromStrict . toBytes
```

Now our oracle takes the session ID and an encryption function,
along with the user-specified content, makes a format request,
compresses it, encrypts it, and reports the length.
For a stream cipher the encryption step is meaningless, but
for a block cipher it will actually mean something.

```haskell
lengthOracle :: (HasBytes sid, HasBytes content)
             => (Bytes -> Bytes) -> sid -> content -> Int
lengthOracle encrypt sid = numBytes . encrypt . compress . formatRequest sid
```

## Stream cipher length attack

Our first attack will be against a stream cipher, say AES-CTR.

```haskell
lengthOracleCTR :: (HasBytes key, HasBytes sid, HasBytes content)
                => key -> sid -> content -> Int
lengthOracleCTR key = lengthOracle ctrEncrypt
 where
  ctrEncrypt = fromJust . encryptCTR key (B.replicate 8 0)
```

The compression attack is not too difficult. It relies on the fact
that the string

    sessionid=BASE64_REPRESENTATION_OF_SESSION_ID

is in the header and thus is being compressed, so if we include the text

    sessionid=B

in the content, it should reuse one more byte and thus compress better than

    sessionid=Q

*Note that this assumes that the session ID doesn't compress well,
i.e. is basically random. If it contains strings that are
elsewhere in the header, like `Cookie` or `hapless`,
it will compress better than random text and this attack will fail.
We really can't control this, so the code here makes no attempt
to compensate for it.*

This is much like the timing attack in Challenge 31;
we can just try every possible character
and take the one whose compressed length is less.

```haskell
compressionAttackStream :: (Bytes -> Int) -> Base64
compressionAttackStream oracle =
```
 
We start with the "known" string, `sessionid=`.

```haskell
  let initState = toBytes "sessionid="
```

To get the next character, we run the oracle on content consisting of
the known prefix then every possible Base64 character.
We then pick the character with the smallest length.

```haskell
      nextChar known = argmin oracle $
                       map (known `BC.snoc`) ('\n' : base64Alphabet)
```  

We get the session ID by taking the newly-found values from the iteration
until we have read the newline,
then dropping off that newline and the `sessionid=` prefix.


```haskell
      sessionID = B.drop (numBytes initState) $ B.init $
                  head $ dropWhile ((/='\n') . BC.last) $
                  iterate nextChar initState
```

We finall convert the session ID from base 64 and return it.

```haskell
  in  fromJust $ mkBase64 $ fromBytes sessionID
```

We'll need to know what characters appear in Base64 encodings;
these are the characters that may be in the session ID.

```haskell
base64Alphabet :: [Char]
base64Alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "+/="
```

## Block cipher length attack

Our second trial is with a block cipher, like CBC.

```haskell
lengthOracleCBC :: (HasBytes key, HasBytes sid, HasBytes content)
                => key -> sid -> content -> Int
lengthOracleCBC key = lengthOracle cbcEncrypt
 where
  cbcEncrypt = fromJust . encryptCBC key (B.replicate 16 0) . padPKCS7 16
```

A compression attack on a block cipher is not nearly as straightforward as on
a stream cipher, because we can only get size differences in lumps of 16 bytes.
Remember our earlier work with block ciphers, though;
we can figure out the actual length by just adding on more content
until the size increases. It's a little trickier this time,
because we have to use extra content that won't compress!

```haskell
compressionAttackBlock :: (Bytes -> Int) -> Base64
compressionAttackBlock oracle =
  let initState = toBytes "sessionid="
```
  
`nextChar` here works much the same as in the previous attack,
except that we augment our oracle call with a little extra, in `findSize`.

```haskell
      nextChar known = argmin findSize $
                       map (known `BC.snoc`) ('\n' : base64Alphabet)
```

`findSize` itself just wraps `findAddedLength`,
from the `BlockTools` module (as used way back in Challenge 16!).
We have to use safe filler text:
printable characters that don't show up in the base-64 alphabet
or in the header, so won't compress.

```haskell
      safeFiller k = BC.pack $ take k "!@#$%^&*()<>[]{}"
```

`findAddedLength` expects an oracle that returns `Bytes`,
not a length.

```haskell
      findSize sid =
        let lenOracle = flip B.replicate 0 . oracle . (<> sid)
        in  findAddedLength safeFiller lenOracle
```

Extracting the session ID is then exactly the same.

```haskell
      sessionID = B.drop (numBytes initState) $ B.init $
                  head $ dropWhile ((/='\n') . BC.last) $
                  iterate nextChar initState
  in  fromJust $ mkBase64 $ fromBytes sessionID
```
