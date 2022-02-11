# Solution to Challenge 42

```haskell
module Challenge42
  (
    rsaVerifySignature_Broken
  , fakeRSASignature
  ) where

import Bytes ( HasBytes(..) )
import Hash ( sha1Hash )
import Math ( iroot )
import PublicKey ( PublicKey(..) )
import PublicKey.RSA ( RSAPublicKey, cryptRSA, rsaBlockSize, sha1Identifier )

import Control.Monad ( guard )

import qualified Data.ByteString as B
```

To validate a signed RSA message, we strip off the padding then verify the hash.
This validator, however, is broken.

```haskell
rsaVerifySignature_Broken :: (HasBytes text, HasBytes sig)
                          => RSAPublicKey -> text -> sig -> Bool
rsaVerifySignature_Broken pk message sig =
```

First, we decrypt the signature and convert it to `Bytes`.
We then have to left-pad it with zeros,
because the `Integer`-to-`Bytes` conversion is going to drop them.

```haskell
  let blockSize = rsaBlockSize pk
      decrypt = cryptRSA (pkKey pk) sig
      block = B.replicate (blockSize - numBytes decrypt) 0 <> toBytes decrypt
```

We're going to step through the block and explicitly look for the pattern

    00 | 01 | FF ... 00 | SHA-1 identifier | hash,

"verifying" if we find it.

```haskell
      searchPattern = sha1Identifier <> toBytes (sha1Hash message)
  in  maybe False id $ do
        guard $ B.take 3 block == B.pack [0,1,0xff]
        let idh = B.tail $ B.dropWhile (>0) $ B.drop 3 block
        pure $ and $ B.zipWith (==) idh searchPattern
```

So why is the validator "broken"?
Remember that in proper padding,
the message is all the way at the right of the block,
with padding filling up the rest.
But what if the block looks like

    00 01 ff .. ff 00 ASN.1 HASH XX XX ..

i.e. has random crap after a valid ASN.1 and hash?
The padding is stripped OK,
but the validator doesn't ensure that the hash is the last thing in the block;
it just validates the hash and is done.
This means that all we need is a block that *starts* with valid padding
and a valid hash; what comes afterwards is meaningless.

Because we're decrypting with the public key (which is 3),
we know that if we find a number which cubes to such a block,
then we're done.
But all we have to do then is take the cube root of one such block;
the nearest integer will (if we have enough junk space remaining)
cube to a nearby block which fits our requirements.

How much junk space do we need? The cube root will have one-third the
number of digits, so two-thirds of our block will be arbitrary.
Unfortunately, given the minimum size of the padding (4 bytes)
and the size of the hash digest and ASN.1 code (at least 34 bytes together),
this doesn't seem to work unless we have
an RSA block size of at least 114 bytes (913 bits).

```haskell
fakeRSASignature :: HasBytes text => RSAPublicKey -> text -> Integer
fakeRSASignature pk message =
```

We construct a message that will pass our broken validator.

```haskell
  let shortBlock = B.concat [ B.pack [0,1,0xff], B.singleton 0
                            , sha1Identifier, toBytes (sha1Hash message) ]
```

We have to fill it out to a full block.
By right-filling with 0xff, we get the largest possible integer that will work;
the floor of its cube root should then hopefully work.

```haskell
      blockSize = rsaBlockSize pk
      filledBlock = shortBlock <>
                    B.replicate (blockSize - numBytes shortBlock) 0xff
```

The faked signature is then the cube root.

```haskell
  in  iroot (3::Integer) $ fromBytes filledBlock
```
