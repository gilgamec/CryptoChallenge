# Solution to Challenge 13

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Challenge13
  (
    Profile, mkProfile, decryptProfile
  , userEmail, userRole
  , fakeAdmin
  ) where

import Bytes ( HasBytes(..), Bytes, splitEnd )
import AES ( encryptECB, decryptECB )
import Padding.PKCS7 ( padPKCS7, validatePKCS7 )
import BlockTools ( alphaFiller
                  , findBlockSize, findAddedLength, findPrefixLength )

import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Network.URL as URL
import qualified Text.Email.Validate as Email
```

## Profiles and profile validation

We create some routines which are (nominally) server-side.
A "profile" is an encrypted string which decrypts to a user profile.
The user profile is an `&`-separated key-value string.

```haskell
newtype Profile = Profile { encryptedProfile :: Bytes }
  deriving (Eq,Show)
```

 We can decrypt the profile and validate its padding.

```haskell
decryptProfile :: HasBytes key => key -> Profile -> Maybe Bytes
decryptProfile key Profile{encryptedProfile = enc} =
  decryptECB key enc >>= validatePKCS7
```

The profile creation function uses the
[email-validate](https://hackage.haskell.org/package/email-validate)
package to sanitize the email address.
It then creates the key-value string, pads, and encrypts it
with the given key to create the profile.

```haskell
mkProfile :: (HasBytes key, HasBytes email) => key -> email -> Maybe Profile
mkProfile key email = do
  sanitized <- Email.canonicalizeEmail (toBytes email)
  fmap Profile $ encryptECB key $ padPKCS7 16 $
    B.concat ["email=",sanitized,"&uid=100&role=user"]
```

The [url](https://hackage.haskell.org/package/url) library
has machinery to read these key-value parameters.

```haskell
userEmail :: HasBytes key => key -> Profile -> Maybe String
userEmail key profile = do
  ps <- decryptProfile key profile
  url <- URL.importURL ("/?" ++ fromBytes ps)
  lookup "email" (URL.url_params url)

userRole :: HasBytes key => key -> Profile -> Maybe String
userRole key profile = do
  ps <- decryptProfile key profile
  url <- URL.importURL ("/?" ++ fromBytes ps)
  lookup "role" (URL.url_params url)
```

## Creating fake admin privileges

The Challenge is to create a fake administrative profile,
given only the ability to create user-level profiles.
The basic idea is to create a normal user-level profile
with an email address which forces the string `user` to the beginning
of a block:

    email=emailaddre ss&uid=xxxx&role= user

Then we create an email address which ends with the string `admin`,
falling at the beginning of a block.

    email=emailendin admin&uid=xxxx&ro le=user

We can then cut and paste that block into the previous profile
to create a profile with the key-value pair `role=admin` and thus
administrative privileges.

    email=emailaddre ss&uid=xxxx&role= admin&uid=xxxx&ro user

The `fakeAdmin` function takes as argument the profile creator.
Our first task is to turn it into a `BlockEncryptOracle` that takes
a string and creates a profile for a valid email address containing that string.

```haskell
fakeAdmin :: (Bytes -> Maybe Profile) -> Profile
fakeAdmin profileFor =
  let myDomain = "example.com" :: Bytes
      oracle str = encryptedProfile $ fromJust $ profileFor $
                   B.concat [ "foo", str, "@", myDomain ]
```

We use the functions from `BlockTools` to find out how much data
is added before and after our user-supplied email address.
We have to use alphabetical filler text, hence `alphaFiller`.

```haskell
      blockSize = findBlockSize alphaFiller oracle
      extraDataSize = findAddedLength alphaFiller oracle
      preDataSize = findPrefixLength blockSize alphaFiller oracle
```

We want to create a normal profile whose last block
is just the encryption of the role `user`.
This text is right at the end of the encrypted string.
We want to add enough padding to the email address to
push this to the beginning of a block;
that is, we want the total size (`extraDataSize + fillerSize`)
to be four bytes longer than one block.

```haskell
      realFillerSize = (4 - extraDataSize) `mod` blockSize
```

We create a profile with a padded email address and split off the last block;
the prefix will end with `&role=`,
then we paste in our own block with role `admin`,
and end with the original end block to match the padding.

```haskell
      (realPrefix, realSuffix) =
        splitEnd blockSize $ oracle $ alphaFiller realFillerSize
```

Now we have to create our evil block.
This will be from a profile for an email address that ends in `admin`.
We then need enough filler text to put our domain `@example.com`
at the end of a block; i.e.
`preDataSize + evilFillerSize + length (@example.com)`
is a whole number of blocks.

```haskell
      evilFillerSize = (-(preDataSize + numBytes myDomain + 1)) `mod` blockSize
      evilBlockPos = preDataSize + evilFillerSize + numBytes myDomain + 1
```

We then create the evil profile and extract the evil block itself.

```haskell
      evilProfile = encryptedProfile $ fromJust $ profileFor $
                    B.concat [ "foo", alphaFiller evilFillerSize
                             , "@", myDomain, "admin" ]
      evilBlock = B.take blockSize $ B.drop evilBlockPos evilProfile
```

Finally, we paste the evil block between our real blocks
to create our fake admin profile.

```haskell
  in  Profile $ realPrefix <> evilBlock <> realSuffix
```
