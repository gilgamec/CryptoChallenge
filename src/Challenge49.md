# Solution to Challenge 49

```haskell
module Challenge49
  (
    CBCDigest, cbcHash

  , BrokenCBCMAC, mkBrokenCBCMAC, validateBrokenCBCMAC
  , Transaction1(..)
  , bankClient1, bankServer1, breakBank1

  , CBCMAC, mkCBCMAC, validateCBCMAC
  , Transaction2(..)
  , bankClient2, bankServer2, breakBank2
  ) where

import Bytes ( HasBytes(..), Bytes, xorb, takeEnd )
import AES ( encryptCBC )
import Hash ( MAC(..), validateMAC )
import Padding.PKCS7 ( padPKCS7 )

import Data.Bifunctor ( second )
import Data.List ( intercalate )
import Data.List.Split ( splitOn )
import Data.Maybe ( fromJust, isJust )

import qualified Data.ByteString as B
```

## Part 1

We can use CBC-AES as a (bad) hash function.
The message is encrypted with a given key and IV,
then the last block of the ciphertext is taken as the message digest.

```haskell
type CBCDigest = Bytes

cbcHash :: (HasBytes key, HasBytes iv, HasBytes text)
        => key -> iv -> text -> CBCDigest
cbcHash key iv text =
  let blockSize = 16
      encrypted = fromJust $ encryptCBC key iv $ padPKCS7 16 text
  in  takeEnd blockSize encrypted
```

In part 1, we use a "broken" CBC-MAC.
It's broken because the client chooses the IV for the CBC encryption;
this gives them complete control over the first block of the decryption.

```haskell
type BrokenCBCMAC text = MAC (text,Bytes) Bytes

mkBrokenCBCMAC :: (HasBytes key, HasBytes text)
               => key -> Bytes -> text -> BrokenCBCMAC text
mkBrokenCBCMAC key iv text = MAC{ macMessage = (text,iv)
                                , macHash = cbcHash key iv text }

validateBrokenCBCMAC :: (HasBytes key, HasBytes text)
                     => key -> BrokenCBCMAC text -> Bool
validateBrokenCBCMAC = validateMAC (\k (t,iv) -> mkBrokenCBCMAC k iv t)
```

We're attacking a "web service" which lets us tranfer money,
with transactions validated by CBC-MAC.
Messages are of the form

    from={from_id}&to={to_id}&amount={amount}

The `Transaction1` data structure holds these values.

```haskell
data Transaction1 = Transaction1 { tx1From, tx1To, tx1Amt :: String }
  deriving (Eq,Ord,Show)

writeTransaction1 :: Transaction1 -> String
writeTransaction1 Transaction1{tx1From=from, tx1To=to, tx1Amt=amt} =
  intercalate "&" [ "from="++from, "to="++to, "amount="++amt ]

parseTransaction1 :: String -> Maybe Transaction1
parseTransaction1 str = vals >>= mkTransaction1 where
  fields = map (second tail . break (=='=')) $ splitOn "&" str
  vals = traverse (flip lookup fields) ["from","to","amount"]
  mkTransaction1 [from,to,amt] =
    Just Transaction1{ tx1From = from, tx1To = to, tx1Amt = amt }
```

Now we write an honest client and server for these transactions.
The bank server validates a message (and applies its transaction).

```haskell
bankServer1 :: HasBytes key => key -> BrokenCBCMAC String -> Maybe Transaction1
bankServer1 key mac
  | validateBrokenCBCMAC key mac = parseTransaction1 (fst $ macMessage mac)
  | otherwise = Nothing
```

The honest client takes a transaction and sends it to the server,
using some random IV.
It will only validate a transaction if the From field matches its user.

```haskell
bankClient1 :: HasBytes key
            => String -> key -> Transaction1 -> Maybe (BrokenCBCMAC String)
bankClient1 user key tx
  | tx1From tx /= user = Nothing
  | otherwise = Just $ mkBrokenCBCMAC key iv $ writeTransaction1 tx
 where
  iv = toBytes "some random text"
```

Now we break the transaction system.

```haskell
breakBank1 :: (Transaction1 -> Maybe (BrokenCBCMAC String))
           -> BrokenCBCMAC String
breakBank1 client =
```

Since we control the IV, this is simple;
we just make a valid transaction which differs on only the first block,
then set the IV to be the XOR of the valid first block
with the evil first block.

The evil transaction transfers money from the victim's account into ours.

```haskell
  let evilTransaction = Transaction1 { tx1From = "victim"
                                     , tx1To = "me"
                                     , tx1Amt = "1000000" }
      evilMessage = writeTransaction1 evilTransaction
```

Look at the evil message split into blocks (including padding):

    from=victim&to=me  &amount=1000000XX

We have to create a valid transaction whose blocks,
except the first, are the same.
The easiest way to do this is to create a transaction
from a dummy account with the same number of characters
as the victim's:

    from=fakeme&to=me  &amount=1000000XX

```haskell
      fakeMe = "fakeme"
      Just goodMAC = client evilTransaction{ tx1From = fakeMe }
```

Since the attacker controls "fakeme",
the client will allow this transaction to be created.

Now we just XOR the first block of the two messages,
together with the current IV, to get the fake IV.

```haskell
      firstBlock = B.take 16 . toBytes
      evilIV = firstBlock evilMessage `xorb`
               firstBlock (fst $ macMessage goodMAC) `xorb`
               snd (macMessage goodMAC)
```

Plugging the evil IV into our good MAC
will result in a validated transaction
making the victim a million bucks poorer.

```haskell
  in  MAC { macMessage = (evilMessage, evilIV)
          , macHash = macHash goodMAC }
```


## Part 2

Part 2 uses a safer (but still bad) "fixed-IV" CBC-MAC.
The IV is no longer part of the authenticated information,
but is basically a secondary key.

```haskell
type CBCMAC text = MAC text CBCDigest

mkCBCMAC :: (HasBytes key, HasBytes iv, HasBytes text)
         => key -> iv -> text -> CBCMAC text
mkCBCMAC key iv text = MAC{ macMessage = text
                          , macHash = cbcHash key iv text }

validateCBCMAC :: (HasBytes key, HasBytes iv, HasBytes text)
               => key -> iv -> CBCMAC text -> Bool
validateCBCMAC = curry $ validateMAC $ uncurry mkCBCMAC
```

The particular IV used in this example is a block of zeros.

```haskell
zeroIV :: Bytes
zeroIV = B.replicate 16 0
```

The transaction format has changed up:
we can now give any number of transactions
from the same person on one line.
We can exploit this with a length extension attack.

```haskell
data Transaction2 = Transaction2
  { tx2From :: String
  , tx2Transactions :: [(String,String)] }
  deriving (Eq,Ord,Show)

writeTransaction2 :: Transaction2 -> String
writeTransaction2 Transaction2{ tx2From = from, tx2Transactions = txs } =
  intercalate "&" [ "from="++from
                  , "tx_list="++(intercalate ";" $
                                 map (\(k,v) -> k++":"++v) txs) ]

parseTransaction2 :: String -> Maybe Transaction2
parseTransaction2 str = vals >>= mkTransaction where
  fields = map (second tail . break (=='=')) $ splitOn "&" str
  vals = traverse (flip lookup fields) ["from","tx_list"]
  mkTransaction [from,tx_list] = Transaction2 from <$> parseTXs tx_list
  parseTXs = Just . map (second tail . break (==':')) . splitOn ";"
```

A new server and client:

```haskell
bankServer2 :: HasBytes key => key -> CBCMAC String -> Maybe Transaction2
bankServer2 key mac
  | validateCBCMAC key zeroIV mac = parseTransaction2 (macMessage mac)
  | otherwise                     = Nothing

bankClient2 :: HasBytes key
            => String -> key -> Transaction2 -> Maybe (CBCMAC String)
bankClient2 user key tx
  | tx2From tx /= user = Nothing
  | otherwise          = Just $ mkCBCMAC key zeroIV $ writeTransaction2 tx
```

And now we break it again.
This time we'll need some valid MACs so we can get
`from=victim` blocks.

```haskell
breakBank2 :: (Transaction2 -> Maybe (CBCMAC String))
           -> [CBCMAC String] -> CBCMAC String
breakBank2 client victimMACs =
```

A valid transaction from the victim (with padding) is something like

    from=victim&tx_l  ist=sister:100XX

We're going to extend this transaction by
gluing on the last blocks of a valid transaction which we create;
this extended transaction will then have the same IV
as our valid one.
We'll keep all but the first block of our own valid transaction
and end up with a message of the form

    from=victim&tx_l  ist=sister:100XX  JUNKJUNKJUNKJUNK  our remaining blocks

We just have to make sure that we have a `;me:1000000`
somewhere in the last blocks of our message.
This, for example, will do it:

    from=fakeme&tx_l  ist=mymom:100;me  :1000000XXXXXXXX

The final message is then going to be something like

    from=victim&tx_l  ist=sister:100XX  GLUEGLUEGLUEGLUE  ist=mymom:100;me :1000000XXXXXXX

```haskell
  let myValidTx = Transaction2
        { tx2From = "fakeme"
        , tx2Transactions = [ ("fakeaccount","100")
                            , ("me","1000000") ] }
      Just myValidMAC = client myValidTx
      (firstBlock,rest) = B.splitAt 16 $ toBytes $ macMessage myValidMAC
```

Now we glue these together.
We have the victim's message and its CBC hash,
which is the ciphertext of its last block.
Our hash is created with an IV of 0,
which is XORed against our first block.
But if we XOR our first block against the victim's hash
and append it to the victim's message,
we get the exact same ciphertext for that block
and the CBC continues as in our own message.

```haskell
      evilMAC victimMAC =
        let evilMessage = B.concat [ padPKCS7 16 $ macMessage victimMAC
                                   , macHash victimMAC `xorb` firstBlock
                                   , rest ]
        in  myValidMAC{ macMessage = fromBytes evilMessage }
```

Note that this technique has the same problem as the CBC forging in Challenge 16:
the glue we create might break the parse (say, with extra & or = signs),
so we may have to try a few times with different valid transactions
until we get one that works.

Fortunately, we don't need an oracle to tell whether
the string will parse;
we have the message itself and can just try to parse it,
looking for `(me,1000000)`

```haskell
      messageParses mac = maybe False id $ do
        tx <- parseTransaction2 (macMessage mac)
        amt <- lookup "me" (tx2Transactions tx)
        pure $ amt == "1000000"
```

Unfortunately, if it doesn't parse we can't do anything about it.
While we exercise some control over the first block of our message,
and thus the junk block, there are lots of bytes that we can't affect
(for instance, our block always starts with `from=`).
We therefore choose to just try again on a new block from the victim.

```haskell
  in  head $ filter messageParses $ map evilMAC victimMACs
```
