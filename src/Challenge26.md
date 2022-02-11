# Solution to Challenge 26

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Challenge26
  (
    Profile, mkProfile, decryptProfile
  , isAdmin
  , fakeAdmin
  ) where

import Bytes ( HasBytes(..), Bytes, xorb )
import AES ( encryptCTR, decryptCTR )

import Data.Maybe ( fromJust, mapMaybe )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Network.URL as URL
```

This is even easier than Challenge 16, where we did CBC bitflipping.

```haskell
newtype Profile = Profile { encryptedProfile :: Bytes }
  deriving (Eq,Show)
```

We can decrypt the profile.
Unlike the CBC case, there is no padding to verify.

```haskell
decryptProfile :: (HasBytes key, HasBytes nonce)
               => key -> nonce -> Profile -> Maybe Bytes
decryptProfile key nonce = decryptCTR key nonce . encryptedProfile
```

We create the profile in the same way:
sanitize the supplied user data
then concatenate and encrypt.

```haskell
mkProfile :: (HasBytes key, HasBytes nonce, HasBytes userdata)
          => key -> nonce -> userdata -> Maybe Profile
mkProfile key nonce userdata = do
  sanitized <- sanitize (toBytes userdata)
  fmap Profile $ encryptCTR key nonce $
    B.concat ["comment1=cooking%20MCs&userdata=",sanitized
             ,"&comment2=%20like%20a%20pound%20of%20bacon"]
 where
  sanitize str | BC.any (\ch -> ch == '&' || ch == '=') str = Nothing
               | otherwise = Just str
```

We once again use the [url](https://hackage.haskell.org/package/url) library
to look for key-value pair `admin=true`.

```haskell
isAdmin :: (HasBytes key, HasBytes nonce)
        => key -> nonce -> Profile -> Bool
isAdmin key nonce profile = maybe False (=="true") $ do
  ps <- decryptProfile key nonce profile
  url <- URL.importURL ("/?" ++ fromBytes ps)
  lookup "admin" (URL.url_params url)
```

Creating a fake admin profile with CTR bitflipping is even easier than with CBC,
since we can just apply our XOR to the block we want to edit,
not to the previous one!
Other than that, the idea is the same.

```haskell
fakeAdmin :: (Bytes -> Maybe Profile) -> Profile
fakeAdmin mkProfile =
```

Our legitimate user profile has user data which is just `&admin=true`,
with the `&` and `=` replaced by an arbitrary character, say `A`.

```haskell
  let evilUserData = "&admin=true"
      legitUserData = BC.map (\c -> if BC.elem c "&=" then 'A' else c)
                             evilUserData
      legitProfileData = encryptedProfile $ fromJust $ mkProfile legitUserData
```

We know the prefix text (we have to in this case,
since there's no blocksize to help us figure it out).
We don't want to alter the prefix or suffix at all,
so we'll XOR them against zeros.

```haskell
      prefixLen = B.length "comment1=cooking%20MCs&userdata="
      suffixLen = numBytes legitProfileData - prefixLen - numBytes evilUserData
      bitmask = B.replicate prefixLen 0 <>
                xorb evilUserData legitUserData <>
                B.replicate suffixLen 0
```

The manipulated profile is then just the bitmask XORed against
the legitimate one.

```haskell
  in  Profile (legitProfileData `xorb` bitmask)
```
