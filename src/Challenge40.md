# Solution to Challenge 40

```haskell
module Challenge40
  (
    broadcastAttack
  ) where

import Math ( iroot, crt )
```

If the same message m is encrypted into three different private keys
(d1,n1), (d2,n2), (d3,n3) to get ciphertexts c1, c2, c3,
then we have

    ci = m^e mod ni;

since e = 3 for all three keys, we can use the Chinese remainder theorem to find

    cx = m^3 mod (n1*n2*n3)

Since m < ni and the nis are about the same size,

    m^3 = (m^3 mod n1*n2*n3) = cx

and the cube root of cx is just m.

The attack takes a number of pairs (ci,ni) and returns the message.

```haskell
broadcastAttack :: [(Integer,Integer)] -> Integer
broadcastAttack = iroot 3 . crt . take 3
```
