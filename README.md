# Haskell Solutions to the Cryptopals Crypto Challenges

The Cryptopals (née Matasano) [Crypto Challenges](https://cryptopals.com),
originally released in 2013,
are a series of coding challenges in cryptography.
They largely involve writing implementations of modern cryptosystems;
then, more importantly, breaking them in well-understood but
(at the time) rarely-implemented ways.
The topics include symmetric- and public-key cryptosystems,
digital signature schemes, cryptographic hashes,
random number generators, timing attacks, and elliptic curves.
All in all, the challenges are a lot of fun that ramp up from
'very easy' to 'somewhat challenging' to 'actually pretty difficult'.

This repository holds my Haskell solutions to all 66 challenges
from all eight problem sets. I've put it online because

1. There aren't many complete solutions; most repositories
   trail off after solving the first couple of problem sets,
   and even the more complete ones usually only have the first
   six problem sets (all that were originally published);
2. Very few solutions are online in Haskell.

My solutions are fairly heavily documented in
[Literate Haskell](https://wiki.haskell.org/Literate_programming),
in Markdown to make them more easily readable from github.
I'm not claiming them as exemplar Haskell style,
just a possible, fairly idiomatic way to implement the challenges.
There is a test for every Challenge, in the `test/` directory,
which confirms that the cryptosystem or exploit works as expected.
Most of the Challenges have a module of their own, in the `src/` directory;
this directory also contains functions and data structures useful in
multiple Challenges, in their own modules.

## Problem Sets

There were originally six sets of eight problems each,
sent by email, one set at a time on demand.
Two more sets were eventually released,
for a total of eight sets and 64 problems.
By the time I did the challenges, several years after the original release,
the first seven sets were available online at [https://cryptopals.com].
Set 8 was not available until it was re-released in 2018,
with two bonus challenges, as part of a political fundraiser;
it's now online at [https://toadstyle.org/cryptopals/].

### Set One: Basics

1. **[Convert hex to base64](https://cryptopals.com/sets/1/challenges/1)**:
   The [`Bytes`](src/Bytes.md) module introduces the `HasBytes` class,
   which describes anything that can be converted back and forth
   from a `ByteString`.
   The modules [`Bytes.Hex`](src/Bytes/Hex.md)
   and [`Bytes.Base64`](src/Bytes/Base64.md)
   contain instances for hex- and base64-encoded strings.

2. **[Fixed XOR](https://cryptopals.com/sets/1/challenges/2)**:
   Adds the `xorb` function, which performs a byte-by-byte XOR
   of two `ByteString`s, to the [`Bytes`](src/Bytes.md) module.

3. **[Single-byte XOR cipher](https://cryptopals.com/sets/1/challenges/3)**:
   We can break a monoalphabetic XOR cipher
   by trying every possible key (there are 256 of them)
   and comparing the decrypted result of each against the expected
   statistical properties of the plaintext.
   In this case, we look just at the distribution of letter frequencies.
   [`XORCipher`](src/XORCipher.md) sets up and breaks
   a monoalphabetic XOR cipher;
   using the tools defined in the [`Distribution`](src/Distribution.md) module,
   candidate plaintexts are compared against an approximate distribution
   of English text defined in
   [`Distribution/English`](src/Distribution/English.md).
   The [`Util`](src/Util.md) module contains various small utility functions.

4. **[Detect single-character XOR](https://cryptopals.com/sets/1/challenges/4)**:
   Just try decrypting every line of the data file.
   Only one will have a decryption to anything like English text.

5. **[Implement repeating-key XOR](https://cryptopals.com/sets/1/challenges/5)**:
   Polyalphabetic XOR is carried out by the function `polyXOR` in
   [`XORCipher`](src/XORCipher.md).

6. **[Break repeating-key XOR](https://cryptopals.com/sets/1/challenges/6)**:
   The function `breakPolyXOR` in [`XORCipher`](src/XORCipher.md)
   decrypts a polyalphabetic XOR cipher by splitting it into multiple
   monoalphabetic XOR ciphers. Splitting is done by `chunksOf`, defined in
   [`Bytes`](src/Bytes.md); some new functions were also added to
   [`Util`](src/Util.md).

7. **[AES in ECB mode](https://cryptopals.com/sets/1/challenges/7)**:
   We use AES primitives from the
   [cryptonite](https://hackage.haskell.org/package/cryptonite) package.
   The appropriate wrapper functions are defined in the
   [`AES`](src/AES.md) module.

8. **[Detect AES in ECB mode](https://cryptopals.com/sets/1/challenges/8)**:
   I don't see how this could be done in general;
   I don't think any "normal" text will have lots of repeated 16-byte blocks.
   But one of the texts in the input file does,
   so (since ECB is a permutation on 16-byte blocks) it must be the text
   the Challenge is talking about.
   The function `countRepeats` in [`Util`](src/Util.md) is
   the only added code.

### Set Two: Block Crypto

9. **[Implement PKCS#7 padding](https://cryptopals.com/sets/2/challenges/9)**:
   The function `padPKCS7` is defined in
   [`Padding.PKCS7`](src/Padding/PKCS7.md).

10. **[Implement CBC mode](https://cryptopals.com/sets/2/challenges/10)**:
    Encrypting is a simple left scan, while decrypting is just a zip.
    `encryptCBC` and `decryptCBC` are defined in [`AES`](src/AES.md).

11. **[An ECB/CBC detection oracle](https://cryptopals.com/sets/2/challenges/11)**:
    Our first chosen-plaintext attack.
    We can tell if an encryption system is using ECB or CBC
    by encrypting a plaintext with many repeated blocks;
    an ECB-encrypted text will then have many repeated
    ciphertext blocks, while the text encrypted with CBC will not.
    This test is carried out by module
    [`Challenge11`](src/Challenge11.md), using new functions from
    the modules [`Random`](src/Random.md) and
    [`BlockTools`](src/BlockTools.md).

12. **[Byte-at-a-time ECB decryption (Simple)](https://cryptopals.com/sets/2/challenges/12)**:
    "Simple" in that we only have to find the unknown *suffix*
    to our chosen plaintext.
    [`Challenge12`](src/Challenge12.md) has the relevant code,
    with helper functions in [`BlockTools`](src/BlockTools.md).
