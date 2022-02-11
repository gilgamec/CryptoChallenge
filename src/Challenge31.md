# Solution to Challenge 31

```haskell
module Challenge31
  (
    fakeSHA1MAC
  , simpleTimingAttack
  ) where

import Bytes ( HasBytes(..), Bytes, Byte )
import Bytes.Hex ( showHex )
import Hash ( MAC(..), SHA1MAC )
import Timing ( TimingOracle )
import Util ( argmaxA )

import qualified Crypto.Hash as H
import qualified Data.ByteString as B

import Debug.Trace ( trace )
```

The timing attack works as follows:
We find the successive bytes of the hash one by one.
If we already know the first `n` bytes,
then we make 256 possible MACs, one for each of the `n+1`st byte.
For the correct one, the validator will make one extra comparison,
which takes longer.
We take the byte from the MAC which took the longest to validate
and repeat to find the `n+2`nd byte.

We need to be able to generate a fake SHA1MAC with the given digest bytes,
which may not be enough to fill up the digest.
The function `fakeSHA1MAC` pads the given `Bytes` out to the digest length.

```haskell
fakeSHA1MAC :: text -> Bytes -> SHA1MAC text
fakeSHA1MAC text fake =
  let digestSize = H.hashDigestSize H.SHA1
      paddedFake = case digestSize - numBytes fake of
        n | n > 0 -> fake <> B.replicate n 0
        _         -> B.take digestSize fake
  in  MAC{ macMessage = text, macHash = fromBytes paddedFake }
```

`getNextByte` finds the next byte of the MAC
by querying all 256 possible extensions
and taking the one that takes the longest.
(This even works when the query succeeds, because
`RequestSucceeded > RequestFailed x` for all `x`!)

```haskell
getNextByte :: Applicative m
            => TimingOracle m (SHA1MAC a) -> a -> Bytes -> m Byte
getNextByte oracle x known =
  let queryOne byte = oracle $ fakeSHA1MAC x $ known `B.snoc` byte
  in  argmaxA queryOne [0..255]
```

Now the timing attack just finds and adds each of the bytes
of the hash in turn.

```haskell
simpleTimingAttack :: Monad m
                   => TimingOracle m (SHA1MAC a) -> a -> m (SHA1MAC a)
simpleTimingAttack oracle x =
```

We add a byte to the known hash by running `getNextByte`
then putting the new byte on the end of the known bytes.
We also include a call to `trace` after each new byte is computed,
letting us keep updated on the progress of the attack.

```haskell
  let addByte known = do
        known' <- B.snoc known <$> getNextByte oracle x known
        pure $ (trace $ "decoded HMAC: " <> showHex known') known'
```

Finding all of the bytes just involves sequencing
the right number of applications of `addByte`,
starting with an empty `known`.

```haskell
      digestSize = H.hashDigestSize H.SHA1
      op = foldl (>>=) (pure B.empty) $ replicate digestSize addByte
```

Finally, we call `fakeSHA1MAC` one last time
with the correct hash to return the fake MAC.

```haskell
  in  fakeSHA1MAC x <$> op
```
