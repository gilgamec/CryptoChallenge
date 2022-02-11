# Communication channel model

This module implements a simple model of a network communication channel
to test signing and authentication protocols.

```haskell
module CommChannel
  (
    Channel, send, recv
  , newChannels, mitmChannels
  , runTwoBots, runThreeBots
  ) where

import Bytes ( HasBytes(..), Bytes )

import Control.Exception ( bracket )
import Control.Concurrent ( Chan, newChan, readChan, writeChan
                          , forkIO, killThread )
import Control.Concurrent.MVar ( MVar, newEmptyMVar, putMVar, readMVar )
```

We implement networked processes with a pair of `MVar` channels.
The first channel is composed of outgoing bytes, the second of incoming bytes.
Because of how `MVar`s work, processes will block
and coordinate sending and receiving messages.

```haskell
data Channel = Channel { inCh, outCh :: Chan Bytes }

send :: HasBytes bytes => Channel -> bytes -> IO ()
send ch = writeChan (outCh ch) . toBytes

recv :: Channel -> IO Bytes
recv ch = readChan (inCh ch)
```

Channels are established in pairs;
the `in` from one is the `out` from the other.

```haskell
newChannels :: IO (Channel,Channel)
newChannels = do
  ch <- Channel <$> newChan <*> newChan
  pure (ch, swapChannel ch)

swapChannel :: Channel -> Channel
swapChannel ch = Channel{ inCh = outCh ch, outCh = inCh ch }
```

In a man-in-the-middle attack there are four channels.

```haskell
mitmChannels :: IO ((Channel,Channel),(Channel,Channel))
mitmChannels = do
  ma <- Channel <$> newChan <*> newChan
  mb <- Channel <$> newChan <*> newChan
  pure ((ma,mb),(swapChannel ma, swapChannel mb))
```  

An agent is just an `IO` operation.

```haskell
type Bot a = IO a
```

Since we execute agents in parallel,
we use `MVar`s to receive their outputs.

```haskell
parBot :: Bot a -> IO (MVar a, Bot ())
parBot bot = do
  mv <- newEmptyMVar
  pure (mv, bot >>= putMVar mv)
```

We then can run some number of bots in parallel,
collecting their outputs.

```haskell
runTwoBots :: Bot a -> Bot b -> IO (a,b)
runTwoBots ba bb = do
  (ma,ba') <- parBot ba
  (mb,bb') <- parBot bb
  bracket (mapM forkIO [ba',bb']) (mapM killThread) $ const $
    (,) <$> readMVar ma <*> readMVar mb

runThreeBots :: Bot a -> Bot b -> Bot c -> IO (a,b,c)
runThreeBots ba bb bc = do
  (ma,ba') <- parBot ba
  (mb,bb') <- parBot bb
  (mc,bc') <- parBot bc
  bracket (mapM forkIO [ba',bb',bc']) (mapM killThread) $ const $
    (,,) <$> readMVar ma <*> readMVar mb <*> readMVar mc
```
