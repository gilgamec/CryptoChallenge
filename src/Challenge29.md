# Solution to Challenge 29

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Challenge29
  (
    Profile, mkProfile, isAdmin
  , fakeAdmin
  ) where

import Bytes ( HasBytes(..), convBytes, Bytes, chunksOf )
import Bytes.Integral ( littleEndian )
import Hash ( SHA1Digest(..), MAC(..), SHA1MAC, mkSHA1MAC, validateSHA1MAC )
import Padding.Hash ( sha1Padding )

import Control.Monad ( guard )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Network.URL as URL

import qualified Crypto.Hash as H
import qualified Data.ByteArray as A

import Unsafe.Coerce ( unsafeCoerce )
```

Our authentication here is a SHA-1 MAC
containing the key-value string `admin=true`.
We build this pretty much the same way as the profiles in previous Challenges,
sanitizing by removing illegal characters from the user data.

```haskell
type Profile = SHA1MAC Bytes

mkProfile :: (HasBytes key, HasBytes userdata)
          => key -> userdata -> Maybe Profile
mkProfile key userdata = do
  sanitized <- sanitize (toBytes userdata)
  pure $ mkSHA1MAC key $
    B.concat ["comment1=cooking%20MCs&userdata=",sanitized
             ,"&comment2=%20like%20a%20pound%20of%20bacon"]
 where
  sanitize str | BC.any (\ch -> ch == '&' || ch == '=') str = Nothing
               | otherwise = Just str
```

To confirm that the user has admin priviliges,
we validate the MAC then look for `admin=true`,
again using [url](https://hackage.haskell.org/package/url).

```haskell
isAdmin :: HasBytes key => key -> Profile -> Bool
isAdmin key mac = maybe False (=="true") $ do
  guard $ validateSHA1MAC key mac
  url <- URL.importURL ("/?" ++ convBytes (macMessage mac))
  lookup "admin" (URL.url_params url)
```

Now we get to forging. We'll need to convert from a SHA-1 digest
back into a SHA-1 context which can continue the hash.

The SHA1 context used in cryptonite is of the form

- size (64 bits, little-endian)
- buffer (64 * 8 = 512 bits)
- h (5 * 32 = 160 bits)
- plus four bytes of padding

size the the total number of bytes in the message so far,
buffer is the unprocessed characters that don't yet make
up a complete 512 bits, and the h part is the digest.

```haskell
newSHA1Context :: Int -> SHA1Digest -> H.Context H.SHA1
newSHA1Context len digest =
```

When we regenerate the context, we need the size and the digest,
and can set the buffer to empty (all zeros).

```haskell
  let bytes = B.concat [ littleEndian 8 len
                       , B.replicate 64 0
                       , dbytes
                       , B.replicate 4 0 ]
```

(On my Linux box, at least, each 32-bit word of the digest is reversed.
This doesn't happen with md4, so who knows what's up.)

```haskell
      dbytes = B.concat $ map B.reverse $ chunksOf 4 $ toBytes digest
```

Since the Context constructor is not exported by cryptonite
(presumably to stop you from mistakenly doing something just like this),
we have to `unsafeCoerce` the data into it.

```haskell
  in  unsafeCoerce (A.convert bytes :: A.Bytes)
```

Now we attempt to forge admin credentials.
Note that creating a new context requires us to know the
length of the message so far, including the length of the key,
which we don't know.
We thus try to forge with every possible key length,
but the true length probably isn't going to be an enormous number.

```haskell
fakeAdmin :: (Profile -> Bool) -> Profile -> Profile
fakeAdmin validate innocent = head $ filter validate $ map forge [0..]
 where
```

Our evil message will be the innocent message,
padded out with the appropriate SHA-1 padding,
then `&admin=true` appended.

```haskell
  innocentText = macMessage innocent
  evilSuffix = "&admin=true"
  evilMessage n = innocentText <> sha1Padding n <> evilSuffix
```

We continue the hash by creating a new context from the existing digest,
then continuing it with the functions exposed by cryptonite.

```haskell
  continueHash n text digest =
    SHA1Digest $ H.hashFinalize $ flip H.hashUpdate text $
    newSHA1Context (n + numBytes (sha1Padding n)) digest
```

The forged MAC just puts the evil message and the evil hash together.

```haskell
  forge keylen =
    let n = keylen + numBytes innocentText
    in  MAC{ macMessage = evilMessage n
           , macHash = continueHash n evilSuffix (macHash innocent) }
``` 
