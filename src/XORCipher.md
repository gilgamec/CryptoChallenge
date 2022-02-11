# Mono- and polyalphabetic XOR ciphers

This module contains functions which perform and break XOR ciphers.

```haskell
module XORCipher
  (
    monoXOR, breakMonoXOR
  , polyXOR, breakPolyXOR
  , breakMultiXOR
  , hammingDistance
  ) where

import Bytes ( HasBytes(..), Bytes, Byte, xor, chunksOf )
import Util ( argmax, argmin, allPairs )
import Distribution ( Distribution, countBytes, logLikelihood )

import Data.Bits ( popCount )

import qualified Data.ByteString as B
```

## Monoalphabetic XOR ciphers

A monoalphabetic cipher simply
XORs every byte in a sequence of bytes by the same value;
it's just a `map` of the `xor` operation.

```haskell
monoXOR :: HasBytes text => Byte -> text -> Bytes
monoXOR x = B.map (xor x) . toBytes
```

To break a monoalphabetic XOR,
we need an expected distribution of the characters in the plaintext.
We then compare every possible decryption (there are only 256)
and choose the one which best matches the expected distribution.

```haskell
breakMonoXOR :: HasBytes text => Distribution -> text -> (Bytes,Byte)
breakMonoXOR dist text =
  let distMatch byte = logLikelihood dist $ countBytes $ monoXOR byte text
      bestByte = argmax distMatch [0..255]
  in  (monoXOR bestByte text, bestByte)
```

## Polyalphabetic XOR ciphers

There are several ways to implement the polyalphabetic XOR:
we could `xorb` the text against many repetitions of the key;
or split the text into key-sized chunks, `xorb` each against the key,
then concatenate them.
Much more efficient than either is using `mapAccumL`,
which doesn't involve allocating any temporary `ByteString`s
and proceeds byte-by-byte along the text,
carrying along the corresponding index into the key.

```haskell
polyXOR :: (HasBytes key, HasBytes text) => key -> text -> Bytes
polyXOR key =
  let key' = toBytes key
      nextIx k = (k + 1) `mod` B.length key'
```

The accumulation function takes the current byte
and the corresponding index into the key,
returning the correct XOR and the next index.

```haskell
      accum k w = (nextIx k, B.index key' k `xor` w)
  in  snd . B.mapAccumL accum 0 . toBytes
```

### Break a polyalphabetic XOR

The Hamming distance between two strings of bits is the number of bits
that differ between them; this is simply the number of 1s in their `xor`.
We compute the distance between two `ByteString`s as the sum of
the Hamming distance between the corresponding bytes, which can be computed
as `popCount` of `xor`.

```haskell
hammingDistance :: (HasBytes a, HasBytes b) => a -> b -> Int
hammingDistance a b = sum $ B.zipWith (\a b -> popCount $ a `xor` b)
                                      (toBytes a) (toBytes b)
```

The description for Challenge 6 suggests using
the Hamming distance to determine the key size:
we are to try the key size which minimizes the average Hamming distance
between successive ciphertext blocks.

Why does this work?
Consider two characters which are k characters apart in the ciphertext.
We don't know the plaintext, but it's probably in English
and so drawn from our English text distribution.
The two characters are thus drawn from

    (english `xor` key[i]) and (english `xor` key[i+k]).

If we have the correct block size k, then key[i] = key[i+k]
and if we XOR the two characters together,
the keys cancel out and we get a value drawn from

    (english `xor` english).

If we have the wrong block size, then key[i] /= key[i+k]
and we get a value drawn from

    (english `xor` english `xor` key[i] `xor` key[i+k]).

(If the key is also English text, then that's

    (english `xor` english `xor` english `xor` english).

The former has a much higher probability of bytes with small bit counts,
in particular a very high probability of zero, and thus has
a small mean bit count (less than 2 per byte).
The distribution of latter is much more uniform,
with a high mean bit count (over 3).

We need quite a few blocks to see the difference between the possible key sizes.
The function `findKeySize` has a parameter for the number of test blocks
to XOR against one another to find our distances.
It takes the key size which has the lowest average Hamming distance
between these blocks.

```haskell
findKeySize :: HasBytes text => Int -> Int -> text -> Int
findKeySize numBlocks maxSize text = argmin avgDist [1..maxSize]
 where
```

The function `avgDist` finds the average distance
between pairs of blocks of the given size `k`.
It first breaks up the text into blocks of the given size
and takes all pairs of them.

```haskell
  avgDist k =
    let blockPairs = allPairs $ take numBlocks (chunksOf k text)
```

Next, it computes the total Hamming distance between each of these pairs.

```haskell
        totalDist = sum $ map (uncurry hammingDistance) blockPairs
```

Finally, it divides by the total number of bytes compared
to get an average distance.

```haskell
    in  fromIntegral totalDist / fromIntegral (k * length blockPairs)
```

Once we've broken our message up into chunks of the correct length,
we have a bunch of texts such that
all of the first bytes are XORed against the same byte B1,
all of the second bytes against the same byte B2, and so on.
We can thus use our function for breaking a monoalphabetic XOR
on the collection of first bytes, second bytes, and so on,
then collate the results.

```haskell
breakMultiXOR :: HasBytes a => Distribution -> [a] -> ([Bytes],Bytes)
breakMultiXOR dist texts =
```

We use `transpose` to collect the first, second, &c. bytes of each block.

```haskell
  let blocks = B.transpose $ map toBytes texts
```

Each of these collections consists of bytes enciphered with the same byte,
so we call `breakMonoXOR` to solve each one.

```haskell
      (ptexts, keyBytes) = unzip $ map (breakMonoXOR dist) blocks
```

Finally, we `transpose` back again for the final solution.

```haskell
  in  (B.transpose ptexts, B.pack keyBytes)
```

Using these two funcions, we can break a polyalphabetic XOR cipher
pretty easily.

```haskell
breakPolyXOR :: HasBytes a => Distribution -> a -> (Bytes,Bytes)
breakPolyXOR dist bs =
```

First, we use `findKeySize` to find the correct key size.

```haskell
  let keySize = findKeySize 8 50 bs
```

Then we break the input into key-sized blocks
and call `breakMultiXOR` to break each of them.

```haskell
      texts = chunksOf keySize bs
      (ptexts, key) = breakMultiXOR dist texts
```

We just concatenate the plaintexts and return them with the key.

```haskell
  in  (B.concat ptexts, key)
```
