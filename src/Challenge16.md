# Solution to Challenge 16

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Challenge16
  (
    Profile, mkProfile, decryptProfile
  , isAdmin
  , fakeAdmin
  ) where

import Bytes ( HasBytes(..), Bytes, xorb, takeEnd, dropEnd )
import AES ( encryptCBC, decryptCBC )
import Padding.PKCS7 ( padPKCS7, validatePKCS7 )
import BlockTools ( BlockEncryptOracle, alphaFiller
                  , findBlockSize, findPrefixLength )

import Data.Maybe ( fromJust, mapMaybe )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Network.URL as URL
```

The setup is essentially the same as Challenge 13,
but using CBC mode rather than ECB.

## Profiles and profile validation

A "profile" is an encrypted string which decrypts to a user profile.
The user profile is an `&`-separated key-value string.

```haskell
newtype Profile = Profile { encryptedProfile :: Bytes }
  deriving (Eq,Show)
```

We can decrypt the profile and validate its padding.

```haskell
decryptProfile :: (HasBytes key, HasBytes iv)
               => key -> iv -> Profile -> Maybe Bytes
decryptProfile key iv Profile{encryptedProfile = enc} =
  decryptCBC key iv enc >>= validatePKCS7
```

Profile creation is essentially the same,
but since we're not creating email addresses,
we use a more liberal custom sanitization function.

```haskell
mkProfile :: (HasBytes key, HasBytes iv, HasBytes userdata)
          => key -> iv -> userdata -> Maybe Profile
mkProfile key iv userdata = do
  sanitized <- sanitize (toBytes userdata)
  fmap Profile $ encryptCBC key iv $ padPKCS7 16 $
    B.concat ["comment1=cooking%20MCs&userdata=",sanitized
             ,"&comment2=%20like%20a%20pound%20of%20bacon"]
```

The only sanitization we perform
is to disallow the & and = characters from the user data.

```haskell
 where
  sanitize str | BC.any (\ch -> ch == '&' || ch == '=') str = Nothing
               | otherwise = Just str
```

We again use the [url](https://hackage.haskell.org/package/url) library
to look for key-value pair `admin=true`.

```haskell
isAdmin :: (HasBytes key, HasBytes iv)
        => key -> iv -> Profile -> Bool
isAdmin key iv profile = maybe False (=="true") $ do
  ps <- decryptProfile key iv profile
  url <- URL.importURL ("/?" ++ fromBytes ps)
  lookup "admin" (URL.url_params url)
```

## Creating a fake profile

This Challenge is actually quite simple to solve.
The "bit-flipping" attack relies on the fact that
each ciphertext block is XORed against the next plaintext block.
This means that if we know the plaintext of a block,
and don't mind munging the plaintext of the preceding block,
then we can just compute the XOR of the existing plaintext
with the desired plaintext, and apply that to the preceding ciphertext.

There's a slight issue that must be dealt with, however.
Since the first block is going to be munged into essentially random bytes,
it's entirely possible that it'll introduce something
that makes the decoding of the string fail;
perhaps one of the bytes will be a %, which introduces
an escaped character code.
The function that does the work thus has a parameter
which slightly changes the user data,
and detects a failed decoding so we can retry if the first attempt
doesn't succeed.
(This seems to happen once every couple of dozen tries,
so modulating only a single byte of the block
should work fine.)

```haskell
fakeAdminTrial :: BlockEncryptOracle -> Int -> (Profile -> Bool) -> Char
               -> Maybe Profile
fakeAdminTrial oracle prefixLen isAdmin ch =
  let blockSize = findBlockSize alphaFiller oracle
```

Our user data will consist of two full blocks:
the second will (after manipulation) end with the `&admin=true` string,
while the first we don't care about and can happily turn to gibberish.
We can use our alphabetic filler text.

```haskell
      legitUserData = BC.singleton ch <> alphaFiller (2 * blockSize - 1)
```  

We then encrypt and split the block that we're XORing against
from the rest of the blocks.

```haskell
      (prefix,rest) = B.splitAt prefixLen $ oracle legitUserData
      (blockToChange, suffix) = B.splitAt blockSize rest
```

We know the current value of the block after `blockToChange`
(the last block of `legitUserData`)
and the desired value (the same, ended by `&admin=true`).
We can thus figure out the XOR to apply.

```haskell
      userBlock = takeEnd blockSize legitUserData
      evilText = "&admin=true"
      evilBlock = dropEnd (numBytes evilText) userBlock <> evilText
      evilXOR = userBlock `xorb` evilBlock
```

We apply this XOR to the `blockToChange` and test the profile.

```haskell
      testProfile = Profile $
                    B.concat [ prefix, blockToChange `xorb` evilXOR, suffix ]
  in  if isAdmin testProfile
      then Just testProfile
      else Nothing
```

The function of the driver function is then twofold.

```haskell
fakeAdmin :: (Bytes -> Maybe Profile) -> (Profile -> Bool) -> Profile
fakeAdmin mkProfile isAdmin =
  let oracle = encryptedProfile . fromJust . mkProfile
      blockSize = findBlockSize alphaFiller oracle
      prefixSize = findPrefixLength blockSize alphaFiller oracle
```

First, it removes the prefix, which just complicates things.
As in Challenge 14, we'll pad it out to a full block
for the version of the oracle we pass to `fakeAdminTrial`.
We will, however, have to also send the length of the prefix
so that function knows where its target blocks are.

```haskell
      prefixPadLength = (-prefixSize) `mod` blockSize
      prefixPad = alphaFiller prefixPadLength
      oracle' = oracle . (prefixPad <>)
      fullPrefixLen = prefixSize + prefixPadLength
```

Second, the driver tries multiple blocks until it gets one that passes.

```haskell
  in  head $ mapMaybe (fakeAdminTrial oracle' fullPrefixLen isAdmin) ['A'..]
```
