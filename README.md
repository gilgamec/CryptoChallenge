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

13. **[ECB cut-and-paste](https://cryptopals.com/sets/2/challenges/13)**:
    Break an encrypted user profile to gain admin privileges.
    This involves writing a profile creator, sanitizer, and validator,
    then a function which breaks it.
    Both are in [`Challenge13`](src/Challenge13.md),
    supported by PKCS#7 validation in [`Padding.PKCS7`](src/Padding/PKCS7.md)
    and new functions from [`BlockTools`](src/BlockTools.md) and
    [`Util`](src/Util.md).

14. **[Byte-at-a-time ECB decryption (Harder)](https://cryptopals.com/sets/2/challenges/14)**:
    "Harder" because we now have to deal with a prefix of unknown length.
    It's not *much* harder, though; it's pretty simple using our existing
    machinery to turn an infix oracle into something that our code from
    Challenge 12 can solve. The function to do that is in the module
    [`Challenge14`](src/Challenge14.md).

15. **[PKCS#7 padding validation](https://cryptopals.com/sets/2/challenges/15)**:
    We already did this to solve Challenge 13, so there's nothing new here.

16. **[CBC bitflipping attacks](https://cryptopals.com/sets/2/challenges/16)**:
    Because in CBC mode we XOR each ciphertext block against the next
    plaintext block, any change to a ciphertext block will cause the
    equivalent XOR to the next plaintext block. We can use this to inject
    all sorts of nasty stuff into plaintext.
    The code is in [`Challenge16`](src/Challenge16.md).

### Set Three: Block and Stream Crypto

17. **[The CBC padding oracle](https://cryptopals.com/sets/3/challenges/17)**:
    My favourite Challenge thus far. Because we know what bytes have to
    appear at the end of valid PKCS#7 padding, we can use a padding oracle,
    which only tells us whether a chosen cipher's padding is valid,
    to completely decrypt any message.
    The function `breakCBCPadding` is defined in module
    [`Challenge17`](src/Challenge17.md).

18. **[Implement CTR, the stream cipher mode](https://cryptopals.com/sets/3/challenges/18)**:
    Encrypt the numbers 0, 1, 2... and use the resulting blocks as a keystream
    to XOR against the message. Encryption and decryption functions added to
    the [`AES`](src/AES.md) module, with some utilities in
    [`Util`](src/Util.md) and [`Bytes.Integral`](src/Bytes/Integral.md).

19. **[Break fixed-nonce CTR mode using substitutions](https://cryptopals.com/sets/3/challenges/19)**:
    AKA "Try to solve this manually so you see how much easier it is
    in Challenge 20 when we do it systematically!"
    I fiddled with this in ghci for a while, and decrypted most of the lines;
    but systematically is better! Thus:

20. **[Break fixed-nonce CTR statistically](https://cryptopals.com/sets/3/challenges/20)**:
    Fixed-nonce CTR is just a repeated keystream, i.e. a polyalphabetic
    XOR cipher. We can use pretty much the same machinery to break it.

21. **[Implement the MT19937 Mersenne Twister RNG](https://cryptopals.com/sets/3/challenges/21)**:
    The Mersenne Twister implementation is in module
    [`MersenneTwister`](src/MersenneTwister.md).

22. **[Crack a MT19937 seed](https://cryptopals.com/sets/3/challenges/22)**:
    AKA "Why you shouldn't use the current time to seed your RNG".
    What code is needed is in [`Challenge22`](src/Challenge22.md).

23. **[Clone an MT19937 RNG from its output](https://cryptopals.com/sets/3/challenges/23)**:
    `cloneMT` added to the [`MersenneTwister`](src/MersenneTwister.md) module.
    Most interesting to me is that you can untemper any 624 successive
    RNG outputs, stick them into a single block, and get a new MT generator
    that reproduces the exact output of the first, but does twisting at
    a different time.

24. **[Create the MT19937 stream cipher and break it](https://cryptopals.com/sets/3/challenges/24)**:
    For the first part of the Challenge, the keyspace is so small that
    we can just brute-force it. Is there some cleverer way to proceed,
    using the properties of MT specifically?
    Code in [`Challenge24`](src/Challenge24.md).

### Set Four: Stream Crypto and Randomness

25. **[Break "random access read/write" AES CTR](https://cryptopals.com/sets/4/challenges/25)**:
    "Because K XOR 0 == K!"
    Code for both editing and breaking the cipher in
    [`Challenge25`](src/Challenge25.md).

26. **[CTR bitflipping](https://cryptopals.com/sets/4/challenges/26)**:
    Even easier than the CBC variant.
    Code in [`Challenge26`](src/Challenge26.md).

27. **[Recover the key from CBC with IV=Key](https://cryptopals.com/sets/4/challenges/27)**:
    Code in [`Challenge27`](src/Challenge27.md).

28. **[Implement a SHA-1 keyed MAC](https://cryptopals.com/sets/4/challenges/28)**:
    The first hash-authentication Challenge.
    SHA-1 hashes and MACs are defined in the module [`Hash`](src/Hash.md).

29. **[Break a SHA-1 keyed MAC using length extension](https://cryptopals.com/sets/4/challenges/29)**:
    This attack is so easy that it's amazing that it's
    "very useful" in the real world.
    The attack is in [`Challenge29`](src/Challenge29.md),
    and it needs the definition of SHA-1 padding from
    [`Padding.Hash`](src/Padding/Hash.md).

30. **[Break an MD4 keyed MAC using length extension](https://cryptopals.com/sets/4/challenges/30)**:
    The same as Challenge 29, with MD4 instead of SHA-1.
    The attack is in [`Challenge30`](src/Challenge30.md);
    we also have to add MD4-specific stuff to
    [`Hash`](src/Hash.md) and [`Padding.Hash`](src/Padding/Hash.md).

31. **[Implement and break HMAC-SHA1 with an artificial timing leak](https://cryptopals.com/sets/4/challenges/31)**:
    This one has a lot of moving parts. Since we need to spin up a
    webserver for testing, there's a new test driver,
    [`TimingTests`](test/TimingTests.hs).
    The webserver validates requests authenticated by an HMAC
    (newly added to [`Hash`](src/Hash.md));
    but the HMAC is compared using `insecure_compare`
    (defined in module [`Timing`](src/Timing.md)),
    which inserts an artificial, tunable pause between the comparison
    of each sequential byte.
    This means that there is a measurable difference between
    the comparison of two strings which match on the first n bytes
    and two which match on the first n+1 bytes.
    The timing attack (implemented in [`Challenge31`](src/Challenge31.md))
    uses this difference to discover the valid MAC by repeated queries.

32. **[Break HMAC-SHA1 with a slightly less artificial timing leak](https://cryptopals.com/sets/4/challenges/32)**:
    The attack from Challenge 31 only works consistently with a
    per-byte delay down to about 30ms. By using multiple queries
    per candidate, and some simple statistics, we can get an attack
    that finds the MAC even with a delay four orders of magnitude smaller.
    I haven't tried running this with a delay below 5µs;
    it *worked* with that delay, but took almost all weekend and over
    a million queries.
    The updated attack is implemented in [`Challenge32`](src/Challenge32.md).

### Set Five: Diffie-Hellman and Friends

33. **[Implement Diffie-Hellman](https://cryptopals.com/sets/5/challenges/33)**:
    Halfway through the Challenges, we come to number-theoretic cryptography.
    One of the most-used modules from now on will be
    [`Modulo`](src/Modulo.md), which implements arithmetic on numbers
    modulo a large integer. Diffie-Hellman itself is described in the module
    [`PublicKey.DiffieHellman`](src/PublicKey/DiffieHellman.md),
    with more general public key infrastructure defined in
    [`PublicKey`](src/PublicKey.md).

34. **[Implement a MITM key-fixing attack on Diffie-Hellman with parameter injection](https://cryptopals.com/sets/5/challenges/34)**:
    We simulate a communication protocol with threads communicating
    via `MVar`s; this machinery is in [`CommChannel`](src/CommChannel.md).
    The protocols for participants A and B, and the man-in-the-middle,
    are defined in [`Challenge34`](src/Challenge34.md).

35. **[Implement DH with negotiated groups, and break with malicious "g" parameters](https://cryptopals.com/sets/5/challenges/35)**:
    More DH parameter meddling. The A and B participants are the ones from
    Challenge 34, while the new eavesdroppers are defined in
    [`Challenge35`](src/Challenge35.md).

36. **[Implement Secure Remote Password (SRP)](https://cryptopals.com/sets/5/challenges/36)**:
    The toy client and server implementations are in
    [`Challenge36`](src/Challenge36.md).

37. **[Break SRP with a zero key](https://cryptopals.com/sets/5/challenges/37)**:
    The (hilariously simple) attacker is in [`Challenge37`](src/Challenge37.md).

38. **[Offline dictionary attack on simplified SRP](https://cryptopals.com/sets/5/challenges/38)**:
    The updated, weaker SRP client and server, plus the impostor server,
    are defined in [`Challenge38`](src/Challenge38.md).

39. **[Implement RSA](https://cryptopals.com/sets/5/challenges/39)**:
    RSA is in [`PublicKey.RSA`](src/PublicKey/RSA.md).

40. **[Implement an E=3 RSA Broadcast attack](https://cryptopals.com/sets/5/challenges/40)**:
    The attack is in [`Challenge40`](src/Challenge40.md),
    but it's basically just integer cube root composed with
    the Chinese remainder theorem. Both are in the module [`Math`](src/Math.md).

### Set Six: RSA and DSA

41. **[Implement unpadded message recovery oracle](https://cryptopals.com/sets/6/challenges/41)**:
    Since RSA encryption is such simple math, it's almost transparent
    to operations like multiplication, which lets us to stuff like recover
    unpadded messages. The code is in [`Challenge41`](src/Challenge41.md).

42. **[Bleichenbacher's e=3 RSA Attack](https://cryptopals.com/sets/6/challenges/42)**:
    Such a cool one! It's so easy to forget that the message has to
    go at the end of the byte string, and you pay for it if you do.
    RSA signatures (including a valid verifier) are implemented in
    [`PublicKey.RSA`](src/PublicKey/RSA.md); they use the PKCS#1 padding scheme,
    implemented in [`Padding.PKCS1`](src/Padding/PKCS1.md).
    The broken validator and an attack against it are in
    [`Challenge42`](src/Challenge42.md).

43. **[DSA key recovery from nonce](https://cryptopals.com/sets/6/challenges/43)**:
    Calling all of these different non-repeated throwaway values "nonces"
    seems really misleading. (And we'll see more "nonces" later on!)
    DSA is implemented in [`PublicKey.DSA`](src/PublicKey/DSA.md);
    code to break the use of a bad "nonce" is in
    [`Challenge43`](src/Challenge43.md).

44. **[DSA nonce recovery from repeated nonce](https://cryptopals.com/sets/6/challenges/44)**:
    Once again, repeat the nonce, give up the game.
    Code is in [`Challenge44`](src/Challenge44.md).

45. **[DSA parameter tampering](https://cryptopals.com/sets/6/challenges/45)**:
    More fun with invalid parameters. Unfortunately for us, setting g to zero
    won't work; if g is zero, then any signature with r = 0 is valid, but the
    [DSA specification](https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.186-4.pdf)
    specifically states that no signature is valid if r is zero.
    Setting g to p+1 works, though.
    Code is in [`Challenge45`](src/Challenge45.md).

46. **[RSA parity oracle](https://cryptopals.com/sets/6/challenges/46)**:
    Really cute, but only a warmup for what's coming next.
    Code is in [`Challenge46`](src/Challenge46.md).

47. **[Bleichenbacher's PKCS 1.5 Padding Oracle (Simple Case)](https://cryptopals.com/sets/6/challenges/47)** and
48. **[Bleichenbacher's PKCS 1.5 Padding Oracle (Complete Case)](https://cryptopals.com/sets/6/challenges/48)**:
    Probably my favourite Challenge in the entire series.
    (Though there are a couple of really good ones coming later too.)
    Relies on the same idea as Challenge 46 - that you can multiply
    'through' RSA encoding - but works by finding factors we can multiply by
    to get valid PKCS#1 padding. When we find a factor that gives us a
    valid padding, we can limit the possible message to some finite
    union of intervals. Challenge 47 is to implement only part of the algorithm -
    basically everything except handling multiple intervals.
    It's actually not much harder to go all the way to the full algorithm,
    so only the complete attack is implemented here;
    intervals, including intersection and union operations,
    are implemented in module [`Interval`](src/Interval.md),
    while the attack itself is in [`Challenge48`](src/Challenge48.md).

    This is the first attack in a while that takes more than a second or so
    to run (at least on my PCs). As such, there's a new test driver.
    It prints out the known upper bound after each iteration, resulting in
    a slow convergence from gibberish to the actual message. The Challenges
    call it a "hollywood style" decryption, so the new driver is
    [`Hollywood`](test/Hollywood.hs).

### Set Seven: Hashes

49. **[CBC-MAC Message Forgery](https://cryptopals.com/sets/7/challenges/49)**:
    Fairly straightforward. CBC-MAC sounds like a not-so-good idea,
    since you can just add in a single glue block of your choice
    to paste two hashes together. Implementation and attacks are in
    [`Challenge49`](src/Challenge49.md).
