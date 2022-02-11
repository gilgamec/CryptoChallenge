# Distrubtion of English letters

This module exports a statistical distribution of the letters of English text.

```haskell
module Distribution.English
  (
    englishDistribution
  ) where

import Distribution ( Distribution )
import Data.Char ( chr )
import qualified Data.Vector as V
```

In order to break some ciphers,
we compare candidate decryptions agains the distribution of English text.
We can create this text by counting characters in a known sample corpus:
in this case, the works of Shakespeare.

```haskell
englishDistribution :: Distribution
englishDistribution = V.generate 256 (getFreq . chr)
 where
  getFreq ch = maybe 0 id $ lookup ch shakespeareFreqs
```

The character frequencies are taken from
[https://opendata.stackexchange.com/a/7043].

```haskell
shakespeareFreqs :: [(Char,Int)]
shakespeareFreqs =
    [ (' ',1293934), ('e',404621), ('t',289975), ('o',281391), ('a',244664)
    , ('h',218406), ('n',215924), ('s',214978), ('r',208894), ('i',198184)
    , ('l',146161), ('d',133779), ('\n',124456), ('u',114818), ('m',95580)
    , ('y',85271), (',',83174), ('.',78025), ('w',72894), ('f',68803)
    , ('c',66688), ('g',57035), ('I',55806), ('b',46543), ('p',46525)
    , ('A',44486), ('E',42583), ('T',39800), ('S',34011), ('v',33989)
    , ('O',33209), ('\'',31069), ('k',29212), ('R',28970), ('N',27338)
    , ('L',23858), ('C',21497), ('H',18462), (';',17199), ('W',16496)
    , ('M',15872), ('D',15683), ('B',15413), ('U',14129), ('P',11939)
    , ('F',11713), ('G',11164), ('?',10476), ('Y',9099), ('!',8844), ('-',8074)
    , ('K',6196), ('x',4688), ('V',3580), ('j',2712), ('q',2404), ('[',2085)
    , (']',2077), ('J',2067), (':',1827), ('Q',1178), ('z',1099), ('9',948)
    , ('1',928), (')',629), ('(',628), ('X',606), ('Z',532), ('"',470)
    , ('<',468), ('>',441), ('2',366), ('3',330), ('0',299), ('4',93)
    , ('5',82), ('_',71), ('6',63), ('*',63), ('7',41), ('8',40), ('|',33)
    , ('&',21), ('@',8), ('/',5), ('}',2), ('=',1), ('%',1), ('~',1)
    , ('#',1), ('`',1) ]
```
