# Solution to Challenge 50

```haskell
module Challenge50
  (
    matchCBCHash
  , fakeJavascript
  ) where

import Bytes ( HasBytes(..), Bytes, xorb, chunksOf )
import Bytes.Hex ( showHex )
import AES ( encryptECB, decryptECB )
import Padding.PKCS7 ( padPKCS7 )

import Data.Char ( isPrint )
import Data.List ( foldl' )
import Data.Maybe ( fromJust )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
```

CBC is really bad as a hash function
because it's easy to create lots of messages
which hash to the same thing.

The `matchCBCHash` function takes a number of prefix blocks
and a number of suffix blocks
and produces a message with the given prefix and suffix
which hashes to the given value.

```haskell
matchCBCHash :: (HasBytes key, HasBytes iv)
             => key -> iv -> Bytes -> [Bytes] -> [Bytes] -> Bytes
matchCBCHash key iv hash prefix suffix =
```

This message we're going to produce consists of the prefix,
then a single block of arbitrary data, then the suffix.
We can select the data block to give us exactly the hash we want.

The ciphertext of a block i is `c_i = ECB(p_i XOR c_(i-1))`.
We want to find the plaintext of our data block, say `p_k`.
Normally this is impossible because it's really hard to invert ECB
without the key.
But since we're using CBC as a hash function,
we know the key!
We can thus easily compute ECB and its inverse:

```haskell
  let ecb    = fromJust . encryptECB key
      invecb = fromJust . decryptECB key
```

Then we can compute

    p_k = c_(k-1) XOR invECB(c_k).

We therefore need two pieces of information:
the ciphertext of the last block of the prefix,
and the ciphertext of the data block itself.

```haskell
      dataBlock = prefixCT `xorb` invecb dataCT
```

The first is easy; it's just the standard CBC iteration.

```haskell
      prefixCT = foldl' (\c p -> ecb $ c `xorb` p) (toBytes iv) prefix
```

The ciphertext of the data block is also straightforward;
since we can invert ECB, we can compute

    c_k = p_(k+1) XOR invECB(c_(k+1))
	    = p_(k+1) XOR invECB(p_(k+2) XOR invECB(p_(k+3) XOR ...)).

This is almost the same operation we used to compute the
CT of the prefix, but in reverse.
We start the iteration with the ciphertext of the last block,
which is the hash we're trying to match.

```haskell
      dataCT = foldr (\p c -> p `xorb` invecb c) hash paddedSuffix
```

Note that the invECB function requires full blocks,
so we have to apply padding to the last suffix block.

```haskell
      paddedSuffix = init suffix ++ chunksOf 16 (padPKCS7 16 $ last suffix)
```

Finally, we just jam the prefix and the suffix togther,
sandwiching our data block in between.

```haskell
  in  B.concat $ prefix ++ [dataBlock] ++ suffix
```

---

We can use `matchCBCHatch` to create Javascript snippets
that hash to anything.
We'll put the glue block inside a Javascript comment;
then whatever junk it contains won't even affect the code.
Unfortunately, there are a few restrictions on what characters
are allowed even in a Javascript comment;
however, we can manipulate one of our given blocks
until we get a glue block that works.

```haskell
fakeJavascript :: (HasBytes key, HasBytes iv)
               => key -> iv -> Bytes -> String -> String
fakeJavascript key iv hash code =
```

Our code will form the "prefix".
Immediately after it, we place a Javascript open-comment mark `/*`.
The prefix has to be an whole number of blocks,
so we insert spaces as needed to make up the space.

```haskell
  let prefixUnpadded = chunksOf 16 $ toBytes $ code ++ "/*"
      prefix = init prefixUnpadded ++ [spacePad $ last prefixUnpadded]
      spacePad block = B.take 16 $ block <> BC.replicate 16 ' '
```

Our suffix will end with the close-comment mark `*/`.
Before that will go more junk characters;
we will change these characters until we get a hash
that is valid Javascript.
We create printable junk characters
by converting an input `Integer` to `Hex`.

```haskell
      suffix :: Integer -> [Bytes]
      suffix n = chunksOf 16 $ BC.pack $ showHex n ++ "*/"
```

There are only a few limitations on the contents
of the glue block.
Any Unicode code point is a valid Javascript character.
We slightly limit it to printable characters;
in addition, we don't want the glue block to contain the
close-comment sequence `*/`,
so we flatly disallow the `*` character as well.

```haskell
      goodBlock = BC.all validNonStar
       where
        validNonStar '*' = False
        validNonStar ch = isPrint ch
```

The glue block is the sixteen bytes after the prefix.

```haskell
      goodGlueBlock = goodBlock . B.take 16 . B.drop (16 * length prefix)
```

Now we just try lots of possible suffixes
until we generate a forgery with a good glue block.

```haskell
  in  fromBytes $ head $ filter goodGlueBlock $
      map (matchCBCHash key iv hash prefix . suffix) [0..]
```
