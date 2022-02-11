# Solution to Challenge 24

There's two challenges here:

1. create the MT19937 stream cipher (and break it), and
2. generate a random token from the cipher (and recognize it).

```haskell
module Challenge24
  (
    mtByteStream, mtXOR
  , mtEncrypt16, breakMTEncrypt16
  , resetToken, isResetToken
  ) where

import Bytes ( HasBytes(..), Bytes, Byte, xorb )
import MersenneTwister ( MTState(..), mtSeed, mtExtract )
import BlockTools ( fillerText, BlockEncryptOracle )

import Data.List ( unfoldr )
import Data.Word ( Word16, Word32 )

import qualified Control.Monad.Random as R
import qualified Data.ByteString as B

import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
```

## Stream cipher

For the stream cipher we just generate a bunch of values
and grab the bottom byte of each.

```haskell
mtByteStream :: MTState -> [Byte]
mtByteStream = map fromIntegral . unfoldr (Just . mtExtract)
```

We can encrypt/decrypt with this by XORing it against the text.

```haskell
mtXOR :: HasBytes text => Word32 -> text -> Bytes
mtXOR key text =
  let keystream = take (numBytes text) $ mtByteStream $ mtSeed key
  in  xorb text (B.pack keystream)
```

Using this we can encrypt with some prefix.
Note that the key is restricted to 16 bits,
as per the Challenge.

```haskell
mtEncrypt16 :: (HasBytes prefix, HasBytes text)
            => Word16 -> prefix -> text -> Bytes
mtEncrypt16 key prefix text =
  mtXOR (fromIntegral key) $ toBytes prefix <> toBytes text
```

Since the key is only 16 bits, we can just brute-force it.
We first encrypt a known plaintext:

```haskell
breakMTEncrypt16 :: BlockEncryptOracle -> Word16
breakMTEncrypt16 cipher =
  let chosenText = fillerText 30
      ciphertext = cipher chosenText
```

We then try decryption with every possible key
until we find one whose suffix is our chosen text.

```haskell
      allKeys = [0 .. maxBound]
      correctKey = B.isSuffixOf chosenText .
                   (\key -> mtXOR (fromIntegral key) ciphertext)
  in  head (filter correctKey allKeys)
```

## Password reset token

The password reset token is a 32-byte chunk of bytes from the byte stream.

```haskell
resetToken :: IO Bytes
resetToken = B.pack . take 32 . mtByteStream . mtSeed . floor <$> getPOSIXTime
```

We can detect the reset token by just constructing a token
for seeds from a bunch of previous times.
We just look at the previous hour.

```haskell
isResetToken :: Bytes -> POSIXTime -> Bool
isResetToken tok now =
  let searchEnd   = 30 + ceiling now
      searchStart = searchEnd - 3600
      mkToken = B.pack . take 32 . mtByteStream . mtSeed
      allTokens = map mkToken [searchStart .. searchEnd]
  in  tok `elem` allTokens
```
