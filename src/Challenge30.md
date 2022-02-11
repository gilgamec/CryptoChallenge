# Solution to Challenge 30

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Challenge30
  (
    Profile, mkProfile, isAdmin
  , fakeAdmin
  ) where

import Bytes ( HasBytes(..), convBytes, Bytes )
import Bytes.Integral ( littleEndian )
import Hash ( MD4Digest(..), MAC(..), MD4MAC, mkMD4MAC, validateMD4MAC)
import Padding.Hash ( md4Padding )

import Control.Monad ( guard )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Network.URL as URL

import qualified Crypto.Hash as H
import qualified Data.ByteArray as A

import Unsafe.Coerce ( unsafeCoerce )
```

This is essentially identical to the SHA-1 version,
but with MD4 instead.

```haskell
type Profile = MD4MAC Bytes

mkProfile :: (HasBytes key, HasBytes userdata)
          => key -> userdata -> Maybe Profile
mkProfile key userdata = do
  sanitized <- sanitize (toBytes userdata)
  pure $ mkMD4MAC key $
    B.concat ["comment1=cooking%20MCs&userdata=",sanitized
             ,"&comment2=%20like%20a%20pound%20of%20bacon"]
 where
  sanitize str | BC.any (\ch -> ch == '&' || ch == '=') str = Nothing
               | otherwise                                  = Just str

isAdmin :: HasBytes key => key -> Profile -> Bool
isAdmin key mac = maybe False (=="true") $ do
  guard $ validateMD4MAC key mac
  url <- URL.importURL ("/?" ++ convBytes (macMessage mac))
  lookup "admin" (URL.url_params url)
```

`newMD4Context` works the same as its SHA-1 equivalent.
The difference is that there are only four 32-bit state words
and thus eight bytes of padding.

(The state words are also no longer reversed,
so we can just directly insert the digest.)

```haskell
newMD4Context :: Int -> MD4Digest -> H.Context H.MD4
newMD4Context len digest =
  let bytes = B.concat [ littleEndian 8 len
                       , B.replicate 64 0
                       , toBytes digest
                       , B.replicate 8 0 ]
  in  unsafeCoerce (A.convert bytes :: A.Bytes)
```

The forger is identical to the SHA-1 version.

```haskell
fakeAdmin :: (Profile -> Bool) -> Profile -> Profile
fakeAdmin validate innocent = head $ filter validate $ map forge [0..]
 where
  innocentText = macMessage innocent
  evilSuffix = "&admin=true"

  evilMessage n = innocentText <> md4Padding n <> evilSuffix
  continueHash n text digest =
    MD4Digest $ H.hashFinalize $ flip H.hashUpdate text $
    newMD4Context (n + numBytes (md4Padding n)) digest

  forge keylen =
    let n = keylen + numBytes innocentText
    in  MAC{ macMessage = evilMessage n
           , macHash = continueHash n evilSuffix (macHash innocent) }
``` 
