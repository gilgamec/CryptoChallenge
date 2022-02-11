# Solution to Challenge 55

Challenge 55 involves replicating the MD4 collision generation technique
from [Wang et al. (2005)](https://dx.doi.org/10.1007/11426639_1).

```haskell
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Challenge55
  (
    MyMD4Digest, myMD4Hash
  , collideMD4
  ) where

import Bytes ( HasBytes(..), Bytes, chunksOf )
import Bytes.Integral ( littleEndian )
import Hash ( md4Hash )
import Padding.Hash ( padMD4 )

import Data.Bits ( Bits(..) )
import Data.List ( foldl', scanl' )
import Data.Traversable ( mapAccumL )
import Data.Word ( Word32 )

import qualified Data.ByteString as B
import qualified Data.Vector as V
```

## Implementing MD4

To get this attack to work,
we have to interfere with the steps of the MD4 hash algorithm,
so we're going to have to implement it in its entirety.
We refer to the
[MD4 specification](https://tools.ietf.org/html/rfc1186).

The MD4 state vector consists of four 32-bit registers,
labelled a through d.

```haskell
data Reg = A | B | C | D deriving (Eq,Ord,Show)
data MD4State = MD4State { a, b, c, d :: {-# UNPACK #-} !Word32 }
  deriving (Eq,Ord,Show)
```

The inital state is a bunch of magic numbers.

```haskell
initMD4State :: MD4State
initMD4State = MD4State
  { a = 0x67452301
  , b = 0xefcdab89
  , c = 0x98badcfe
  , d = 0x10325476 }
```

The MD4 digest is just the MD4 state itself.
We give it a `HasBytes` instance.

```haskell
type MyMD4Digest = MD4State
instance HasBytes MD4State where
```

The state is output as four little-endian words.

```haskell
  toBytes MD4State{a=a,b=b,c=c,d=d} =
    B.concat $ map (littleEndian 4) [a,b,c,d]
```

It is thus input by reversing the bytes and converting back to words.

```haskell
  fromBytes bs =
    let [a,b,c,d] = map (fromBytes . B.reverse) $ chunksOf 4 bs
    in  MD4State{a=a,b=b,c=c,d=d}
```

It is also always the same length: 16 bytes.

```haskell
  numBytes _ = 16
```

A pass of MD4 over a 64-byte block consists of three *rounds*,
each of 16 *steps*.
A step is of the form

    r1 <- (r1 + f(r2,r3,r4) + w[i] + k) `rotateL` s

where r[1234] is some permutation of registers abcd,
f and k depend on the round,
and w[i] is an element of the input block,
which has been split into 16 32-bit words.

- There are only four possible register orderings, one for updating
  each of the four registers.  They always proceed A-D-C-B,
  with four cycles per round.
- The values of the shift s for each step also come in a cycle of four,
  which differs from round to round.
- The values of i for each step in a round are from a permutation
  of [0..15] which depends on the round.

We can describe a round by its f, k, shift cycle, and permutation of i.

```haskell
data MD4Round = MD4Round
  { roundF :: Word32 -> Word32 -> Word32 -> Word32
  , roundK :: Word32
  , roundShifts :: [Int]
  , roundIndices :: [Int] }
```

The three possible values for f are simple bitwise functions.

```haskell
md4F1, md4F2, md4F3 :: Word32 -> Word32 -> Word32 -> Word32
md4F1 x y z = (x .&. y) .|. (complement x .&. z)
md4F2 x y z = (x .&. y) .|. (x .&. z) .|. (y .&. z)
md4F3 x y z = x `xor` y `xor` z
```

The constants k are magic numbers.

```haskell
md4K1, md4K2, md4K3 :: Word32
md4K1 = 0x00000000
md4K2 = 0x5A827999
md4K3 = 0x6ED9EBA1
```

We can now describe the three rounds.

```haskell
md4Round1 = MD4Round
  { roundF = md4F1, roundK = md4K1, roundShifts = [3,7,11,19]
  , roundIndices = [0..15] }
md4Round2 = MD4Round
  { roundF = md4F2, roundK = md4K2, roundShifts = [3,5,9,13]
  , roundIndices = [0,4..12] ++ [1,5..13] ++ [2,6..14] ++ [3,7..15] }
md4Round3 = MD4Round
  { roundF = md4F3, roundK = md4K3, roundShifts = [3,9,11,15]
  , roundIndices = [0,8,4,12] ++ [2,10,6,14] ++ [1,9,5,13] ++ [3,11,7,15] }
```

The differences between the different steps in a round are
which register we are updating,
how large the shift is,
and which word of the input block we are reading.

```haskell
data MD4Step a = MD4Step
  { md4F :: Word32 -> Word32 -> Word32 -> Word32
  , md4K :: Word32
  , md4Reg :: Reg
  , md4Shift :: Int
```

We can store the index i or the input word w[i], after lookup;
for this reason, we let `md4Value` be a parametric type.

```haskell
  , md4Value :: a }
  deriving Functor
```

We can now create an explicit representation of every step in the computation.
Here the parameter of `MD4Step` is the index i;
we can later get data words by e.g. `fmap (ws!!)`.

```haskell
md4Steps :: [MD4Step Int]
md4Steps = [ MD4Step (roundF r) (roundK r) g s i
           | r <- [md4Round1, md4Round2, md4Round3]
           , (g,s,i) <- zip3 (cycle [A,D,C,B])
                             (cycle $ roundShifts r)
                             (roundIndices r) ]
```

Given the description of a step (with the value w[i] rather than i)
we can apply the step to a register state.

```haskell
md4Step :: MD4State -> MD4Step Word32 -> MD4State
md4Step state step =
  let MD4State{a=a,b=b,c=c,d=d} = state
      MD4Step{ md4F = f, md4K = k, md4Shift = s, md4Value = wi } = step
      mdOp r1 r2 r3 r4 = (r1 + f r2 r3 r4 + wi + k) `rotateL` s
  in  case md4Reg step of
        A -> state{ a = mdOp a b c d }
        D -> state{ d = mdOp d a b c }
        C -> state{ c = mdOp c d a b }
        B -> state{ b = mdOp b c d a }
```

To apply MD4 to an entire 64-byte chunk,
we have to apply every step in order.

```haskell
mdChunk :: MD4State -> V.Vector Word32 -> MD4State
mdChunk st ws =
```

The description of the steps is in `md4Steps`.
We first use an `fmap` to change each index in the step descriptions
to the actual data word referred to.

```haskell
  let steps = map (fmap (ws V.!)) md4Steps
```

We then use a fold to apply each step to the state in turn.

```haskell
      newSt = foldl' md4Step st steps
```

Finally, we add the old and new values of each register
to generate the new state.

```haskell
  in  MD4State { a = a st + a newSt
               , b = b st + b newSt
               , c = c st + c newSt
               , d = d st + d newSt }
```

We need to convert between 64-byte blocks and `Vector`s of sixteen 32-bit words.

```haskell
chunkBytes :: Bytes -> V.Vector Word32
chunkBytes = V.fromListN 16 . map (fromBytes . B.reverse) . chunksOf 4

unchunkBytes :: V.Vector Word32 -> Bytes
unchunkBytes = B.concat . map (littleEndian 4) . V.toList
```

Now we can produce an MD4 checksum.

```haskell
myMD4Hash :: HasBytes text => text -> MyMD4Digest
myMD4Hash text =
```

We apply MD4 padding and split into 64-byte data blocks,
then into data `Vector`s.

```haskell
  let chunks = map chunkBytes $ chunksOf 64 $ padMD4 text
```

We then use a fold to process each chunk in turn and compute our final state.

```haskell
  in  foldl' mdChunk initMD4State chunks
```

## Generating MD4 collisions

Now we look at finding MD4 collisions.
This is all after
[the paper by Wang et al. in Eurocrypt 2005](https://dx.doi.org/10.1007/11426639_1).

The idea is that we take a 64-byte message block and manipulate its bits
so that certain bitwise identities hold for the state registers
at various points in the digest computation.
There is then a very high probability that a closely related block
will have a colliding MD4 hash.

```haskell
collideMD4 :: Bytes -> Maybe (Bytes,Bytes)
collideMD4 chunk =
```

We modify the block with the yet-to-be-defined functions
`modifyFirstRound` and `modifySecondRound`.

```haskell
  let m = modifySecondRound . modifyFirstRound $ chunkBytes chunk
      m0 = unchunkBytes m
```

The related block is then

```haskell
      m1 = unchunkBytes $ V.zipWith (+) m dm
```

where the difference vector is (from the paper)

    dm1 = 2^31, dm2 = 2^31 - 2^28, dm12 = -2^16:

```haskell
      dm = V.fromListN 16 [0,2^31,2^31-2^28,0,0,0,0,0,0,0,0,0,-2^16,0,0,0]
```

We then just ensure that the two blocks differ
and their hashes are the same,
then return them.

```haskell
  in  if m0 /= m1 && md4Hash m0 == md4Hash m1
      then Just (m0,m1)
      else Nothing
```

So, what are the mysterious modifications that we have to make to the block
so that it is vulnerable to generating collisions?
Essentially, in each step of hash generation,
there are certain conditions on specific bits of the state register.
We run the step, modify the state to ensure that the conditions are met,
then determine what the data word would have had to have been
to create that state in normal evaluation.
The data word is modified and we move to the next step.

All of the requirements (that we're going to care about)
are of the form

    pi[b] = 0, pi[b] = 1, or pi[b] = q[b]

where p is the register updated in that step, b is the bit index,
and q is another register used in the computation of p.
i represents p's *generation*, the number of times it has been updated
for this chunk.
The initial state has generation 0,
the values for registers a, d, c, and b created in the first four steps
have generation 1, and so on.

We represent a bit value by its register and bit index.

```haskell
data BitValue = BV Reg Int deriving (Eq,Ord,Show)
```

A bit constraint is a zero, a one, or an indication to
look at a previous register.

```haskell
data BitConstraint = Zero | One | Reg Reg deriving (Eq,Ord,Show)
```

A bit condition is a pairing of the two.

```haskell
data CollideCondition = CC BitValue BitConstraint deriving (Eq,Ord,Show)
```

Now the constraints from Table 6 in the paper.
If *all* of these 121 constraints are satisfied,
then the block is *guaranteed* to have a hash collision.
We copy only the constraints on the first 22 rounds;
after that, we start seeing constraints of the form pi[b] = NOT q[b].
However, it isn't really necessary to maintain 22 rounds of constraints;
even enforcing only 18 rounds of constraints,
there are only 2^16 or so unenforced bits,
so we can still find collisions in only a few seconds.

```haskell
conditions :: V.Vector [CollideCondition]
conditions =
```

We define a few functions to ease our data entry.
First, the bit indices in the paper count from one,
so we subtract 1 to zero-index the bit indices.
The data in the paper also includes the generation,
which we don't need and remove.

```haskell
  let v r _i b = BV r (b-1)
```

Second, we create an inline alias for the constraint constructor `CC`.

```haskell
      (.==) = CC
```

Now we proceed to entering the table.
The ith entry lists all of the constraints for step i of the computation.
  
```haskell
  in  V.fromList
      [ [ v A 1 7  .== Reg B ]
      , [ v D 1 7  .== Zero , v D 1 8  .== Reg A, v D 1 11 .== Reg A ]
      , [ v C 1 7  .== One  , v C 1 8  .== One  , v C 1 11 .== Zero
      , v C 1 26 .== Reg D ]
      , [ v B 1 7  .== One  , v B 1 8  .== Zero , v B 1 11 .== Zero
      , v B 1 26 .== Zero  ]
      , [ v A 2 8  .== One  , v A 2 11 .== One  , v A 2 26 .== Zero
      , v A 2 14 .== Reg B ]
      , [ v D 2 14 .== Zero , v D 2 19 .== Reg A, v D 2 20 .== Reg A
      , v D 2 21 .== Reg A, v D 2 22 .== Reg A, v D 2 26 .== One   ]
      , [ v C 2 13 .== Reg D, v C 2 14 .== Zero , v C 2 15 .== Reg D
      , v C 2 19 .== Zero , v C 2 20 .== Zero , v C 2 21 .== One
      , v C 2 22 .== Zero  ]
      , [ v B 2 13 .== One  , v B 2 14 .== One  , v B 2 15 .== Zero
      , v B 2 17 .== Reg C, v B 2 19 .== Zero , v B 2 20 .== Zero
      , v B 2 21 .== Zero , v B 2 22 .== Zero  ]
      , [ v A 3 13 .== One  , v A 3 14 .== One  , v A 3 15 .== One
      , v A 3 17 .== Zero , v A 3 19 .== Zero , v A 3 20 .== Zero
      , v A 3 21 .== Zero , v A 3 23 .== Reg B, v A 3 22 .== One     -- sic
      , v A 3 26 .== Reg B ]
      , [ v D 3 13 .== One  , v D 3 14 .== One  , v D 3 15 .== One
      , v D 3 17 .== Zero , v D 3 20 .== Zero , v D 3 21 .== One
      , v D 3 22 .== One  , v D 3 23 .== Zero , v D 3 26 .== One
      , v D 3 30 .== Reg A ]
      , [ v C 3 17 .== One  , v C 3 20 .== Zero , v C 3 21 .== Zero
      , v C 3 22 .== Zero , v C 3 23 .== Zero , v C 3 26 .== Zero
      , v C 3 30 .== One  , v C 3 32 .== Reg D ]
      , [ v B 3 20 .== Zero , v B 3 21 .== One  , v B 3 22 .== One
      , v B 3 23 .== Reg C, v B 3 26 .== One  , v B 3 30 .== Zero
      , v B 3 32 .== Zero  ]
      , [ v A 4 23 .== Zero , v A 4 26 .== Zero , v A 4 27 .== Reg B
      , v A 4 29 .== Reg B, v A 4 30 .== One  , v A 4 32 .== Zero  ]
      , [ v D 4 23 .== Zero , v D 4 26 .== Zero , v D 4 27 .== One
      , v D 4 29 .== One  , v D 4 30 .== Zero , v D 4 32 .== One   ]
      , [ v C 4 19 .== Reg D, v C 4 23 .== One  , v C 4 26 .== One
      , v C 4 27 .== Zero , v C 4 29 .== Zero , v C 4 30 .== Zero  ]
      , [ v B 4 19 .== Zero , v B 4 26 .== One  , v B 4 27 .== One
      , v B 4 29 .== One  , v B 4 30 .== Zero ]
 
      , [ v A 5 19 .== Reg C, v A 5 26 .== One  , v A 5 27 .== Zero
      , v A 5 29 .== One  , v A 5 32 .== One ]
      , [ v D 5 19 .== Reg A, v D 5 26 .== Reg B, v D 5 27 .== Reg B
      , v D 5 29 .== Reg B, v D 5 32 .== Reg B ]
      , [ v C 5 26 .== Reg D, v C 5 27 .== Reg D, v C 5 29 .== Reg D
      , v C 5 30 .== Reg D, v C 5 32 .== Reg D ]
      , [ v B 5 29 .== Reg C, v B 5 30 .== One  , v B 5 32 .== Zero  ]
      , [ v A 6 29 .== One  , v A 6 32 .== One ]
      , [ v D 6 29 .== Reg B ]
      , [ v C 6 29 .== Reg D ]
      ]
```

Phew! Now, let's go about enforcing these constraints.
First, some prepatory functions.

We have a getter and a setter that look at the state register
corresponding to a given `Reg`.
 
```haskell
getReg :: Reg -> MD4State -> Word32
getReg A MD4State{a=a} = a
getReg B MD4State{b=b} = b
getReg C MD4State{c=c} = c
getReg D MD4State{d=d} = d

setReg :: Reg -> Word32 -> MD4State -> MD4State
setReg A a' st = st{ a = a' }
setReg B b' st = st{ b = b' }
setReg C c' st = st{ c = c' }
setReg D d' st = st{ d = d' }
```

We can combine this with the bit functions to create
a getter and setter for a single bit value.

```haskell
getBV :: BitValue -> MD4State -> Bool
getBV (BV r i) = flip testBit i . getReg r

setBV :: BitValue -> Bool -> MD4State -> MD4State
setBV (BV r i) v st =
  let w = getReg r st
      w' = (if v then setBit else clearBit) w i
  in  setReg r w' st
```

The helper function `vValue` tells us what the target value of the bit is.
For conditions on `Zero` or `One`, this is easy.

```haskell
vValue :: CollideCondition -> MD4State -> Bool
vValue (CC _ Zero) _ = False
vValue (CC _ One ) _ = True
```

For a condition based on another register,
we have to test that register's bit.

```haskell
vValue (CC (BV _ i) (Reg r)) st = getBV (BV r i) st
```

We can grab the entire state evolution using a scan.
(This is essentially just `mdChunk` with `scanl` instead of `foldl`,
and not performing the state addition at the end.)

```haskell
stateEvolution :: MD4State -> V.Vector Word32 -> [MD4State]
stateEvolution st ws =
  let steps = map (fmap (ws V.!)) md4Steps
  in  tail $ scanl' md4Step st steps
```

Enforcing a given constraint entails looking up a target bit value
and either setting or clearing a bit in the given register.

```haskell
enforceReg :: CollideCondition -> MD4State -> MD4State
enforceReg cc@(CC bv _) st = setBV bv (vValue cc st) st
```

Now we can enforce a condition on the target register.
How do we modify the data words to accomodate this?
Recall that each MD4 step alters a register r to

    r' = (r + f o p q + wi + k) `rotateL` s

Clearly, we can invert this to see that

    wi = (r' `rotateR` s) - r - f o p q - k.

`md4Unstep` performs this procedure,
undoing an MD4 step to determine the data word
that would have created the state change performed.

```haskell
md4Unstep :: MD4State -> MD4Step a -> Word32 -> Word32
md4Unstep state step oldReg =
  let MD4State{a=a,b=b,c=c,d=d} = state
      MD4Step{ md4F = f, md4K = k, md4Shift = s } = step
      mdUnop r1 r2 r3 r4 = (r1 `rotateR` s) - oldReg - f r2 r3 r4 - k
  in  case md4Reg step of
        A -> mdUnop a b c d
        D -> mdUnop d a b c
        C -> mdUnop c d a b
        B -> mdUnop b c d a
```

### First-round modifications

Now we can perform all of the modifications of the
sixteen steps that make up the first round.
These modifications are simpler because
each step depends on exactly one of the words of the data chunk;
each word can thus be modified without any regard for the values
in previous steps.

The function `condStep` performs a single MD4 step
subject to a list of bit conditions.
It takes the current state and a description of the step,
along with the unmodified original data word,
and produces the new state (with enforced conditions)
and the modified data word that produces that state.

```haskell
condStep :: MD4State -> (MD4Step Word32, [CollideCondition])
         -> (MD4State, Word32)
condStep state (step,conds) =
```

The state after processing the unmodified data word is

```haskell
  let cState = md4Step state step
```

We enforce each of the conditions in turn,
creating a state that satisfies all of them.

```haskell
      vState = foldr ($) cState $ map enforceReg conds
```

We can now reverse the step to find the new value of the data word.

```haskell
      oldReg = getReg (md4Reg step) state
      w = md4Unstep vState step oldReg
```

Finally, we return the valid state and the modified data word.

```haskell
  in  (vState, w)
```

We perform our first-round modifications, then,
by simply running `condStep` for all of the first-round steps.

```haskell
modifyFirstRound :: V.Vector Word32 -> V.Vector Word32
modifyFirstRound ws =
```

The updated data words are all created and collected in a single `mapAccumL`,
with the MD4 state as the accumulator.

```haskell
  snd $ mapAccumL condStep initMD4State $
```

The input vector, pairing steps and conditions, is build it with `zipWith3`.

```haskell
  V.zipWith3 (\s c w -> (w <$ s, c))
             (V.fromListN 16 md4Steps)
             conditions
             ws
```

### Second-round modifications

Enforcing the conditions for the rest of the steps is a tricky procedure
because they use data words we've already changed;
we have to be careful that we don't undo a change performed in a previous step.
For the first three steps of the second round
(which is all this code implements), this isn't too bad;
we can ensure that our state is unchanged by rewinding multiple
steps and modifying multiple message words.

Take round 17 as an example.
We enforce conditions on the state in round 17 by modifying its message word,
which happens to be message word 0 again.
However, we've already modified word 0 in the first round,
and a further modification to this word
will cause the register computed in that step to be different than before.
Rather than restarting the entire process, however,
it turns out to be possible to ensure that the MD4 state
eventually ends up in the same place.

We can write an MD4 state by the generation of each register.
The initial state is (a0,b0,c0,d0),
the next state (a1,b0,c0,d0), the next (a1,b0,c0,d1),
the fifteenth (a4,b3,c4,d4), and so on.
After step 17, the state is (a5,b4,c4,d4).
In order to satisfy a constraint on a5, we have to modify w0.
But w0 is also used in step 1 to produce a1;
a modified w0 will produce a modified register a1'.

Notice, however, that a1 is only in the state
for the first four rounds;
after round 5, the state is (a2,b1,c1,d1).
We can keep the state at round 5 (and thus all further rounds) the same
by using `md4Unstep` to find the modified message words w1 through w4
that produce the succession of states

    (a1',b0,c0,d1), (a1',b0,c1,d1), (a1',b1,c1,d1), (a2,b1,c1,d1).

If the state after round 5 is the same, then we're OK,
and the rest of the computation will proceed as before.

These multi-step modifications are done individually for each condition.

```haskell
multiStepModification :: Int -> CollideCondition -> V.Vector Word32
                      -> V.Vector Word32
multiStepModification stepIdx cc ws =
```

We need the state evolution for the old message words.

```haskell
  let statesOld = stateEvolution initMD4State ws
```

To perform an unstep, we'll need the states before and after `stepIdx`.

```haskell
      prevState:state:_ = drop (stepIdx - 1) statesOld
```

We change the value of the data word modified in this step.

```haskell
      step@MD4Step{ md4Reg = reg, md4Value = idx } = md4Steps !! stepIdx
      w0' = md4Unstep (enforceReg cc state) step (getReg reg prevState)
```

Now we have to undo four more steps
to find new values for the next few message words.
We can (thanks to laziness) build the new message vector:

```haskell
      ws' = ws V.// zip [idx..] [w0',w1',w2',w3',w4']
      statesNew = stateEvolution initMD4State ws'
```

We grab the old and new register values from old and new states:

```haskell
      MD4State{a=a1',b=b0,c=c0,d=d0} = statesNew !! idx
      MD4State{a=a2 ,b=b1,c=c1,d=d1} = statesOld !! (idx + 4)
```

The altered message words are computed from the desired state transitions:

```haskell
      w1' = md4Unstep (MD4State a1' b0 c0 d1) (md4Steps !! (idx + 1)) d0
      w2' = md4Unstep (MD4State a1' b0 c1 d1) (md4Steps !! (idx + 2)) c0
      w3' = md4Unstep (MD4State a1' b1 c1 d1) (md4Steps !! (idx + 3)) b0
      w4' = md4Unstep (MD4State a2  b1 c1 d1) (md4Steps !! (idx + 4)) a1'
```

We then return the new message vector.

```haskell
  in  ws'
```

The multi-step modification above works for enforcing conditions on step 17,
because fixing these conditions does not change the conditions
we enforced during the first round.
Unfortunately, the same is not true of conditions on steps 18 and 19;
fixing them can break the work we did earlier.
We have a few options here:

1. Apply what Wang et al. call "more precise modification".
   These involve normal multi-step modifications,
   along with extra conditions to be applied in earlier rounds.
   The paper doesn't really go into how to derive these conditions,
   so I'm going to ignore them.
2. Ignore the conditions that can break earlier work.
   This leaves us with 21 unenforced constraints,
   or about one collision per two million attempts.
   This is still pretty fast, but we can do much better.
3. Just apply the normal multi-step modifications, ignoring the
   possibility of breaking things.
   This is perfectly feasible; enforcing the five step 18
   conditions can potentially break only three conditions
   in steps 5 and 6, and enforcing the five conditions
   on step 19 can only break three conditions on step 9.
   We are thus on average seven bits ahead,
   leaving us with about one collision per 150,000 trials.
4. Apply the multi-step modifications; then, 
   if some first-round conditions were broken,
   fix them up and restart the second-round modifications.
   I suppose this has the potential of falling into an infinite loop,
   but I have never witnessed it in hundreds of thousands of trials.
   Each trial takes longer, but we can get down to eleven unenforced constraints,
   leaving us with a collision every couple of thousand attempts.

Option 2 is the simplest to implement:

```haskell
modifySecondRound2 :: V.Vector Word32 -> V.Vector Word32
modifySecondRound2 =
  let funs = map (multiStepModification 16) (conditions V.! 16) ++
             map (multiStepModification 17) (conditions V.! 17) ++
             map (multiStepModification 18) (conditions V.! 18)
  in  foldl (flip (.)) id funs
```

For the tests, however, I'm going to implement option 4,
which seems faster (even if it has the potential of looping indefinitely).
We'll need a function to test if the first-round conditions are satisfied
after second-round modification.

`satsifies` tests if the given condition is satisfied
in the given state.

```haskell
satisfies :: MD4State -> CollideCondition -> Bool
satisfies st cc@(CC bv _) = vValue cc st == getBV bv st
```

During multi-step modifications for steps 18 and 19,
the only modified registers are a2 and a3.
This means that only a small subset of the bit conditions
have to be considered.

```haskell
aConditions :: V.Vector [CollideCondition]
aConditions = V.imap aSubset $ V.take 16 conditions
 where
```

First, all of the conditions on the as themselves
should be rechecked.

```haskell
  aSubset i cs
    | i `rem` 4 == 0 = cs
```

Second, any conditions on the later registers
that reference A should also be kept.

```haskell
    | otherwise = filter (\case
                             CC _ (Reg A) -> True
                             _ -> False) cs
```

We can check for these conditions during the multi-step modification itself.
This modified function returns, in addition to the altered data block,
a flag reporting whether the conditions were still satisfied by the
new states.

```haskell
multiStepModification' :: Int -> CollideCondition -> V.Vector Word32
                       -> (V.Vector Word32,Bool)
multiStepModification' stepIdx cc ws =
  let statesOld = stateEvolution initMD4State ws
      prevState:state:_ = drop (stepIdx - 1) statesOld
      step@MD4Step{ md4Reg = reg, md4Value = idx } = md4Steps !! stepIdx

      w0' = md4Unstep (enforceReg cc state) step (getReg reg prevState)
      ws' = ws V.// zip [idx..] [w0',w1',w2',w3',w4']
      statesNew = stateEvolution initMD4State ws'
      MD4State{a=a1',b=b0,c=c0,d=d0} = statesNew !! idx
      MD4State{a=a2 ,b=b1,c=c1,d=d1} = statesOld !! (idx + 4)

      w1' = md4Unstep (MD4State a1' b0 c0 d1) (md4Steps !! (idx + 1)) d0
      w2' = md4Unstep (MD4State a1' b0 c1 d1) (md4Steps !! (idx + 2)) c0
      w3' = md4Unstep (MD4State a1' b1 c1 d1) (md4Steps !! (idx + 3)) b0
      w4' = md4Unstep (MD4State a2  b1 c1 d1) (md4Steps !! (idx + 4)) a1'
```

We can check our conditions by checking the A-conditions for
the four modified states (`statesNew` at indices `idx` through `idx + 3`).

```haskell
      satVec = V.take 4 $ V.drop idx $
               V.zipWith (\st -> all (st `satisfies`))
                         (V.fromList statesNew) aConditions
```

We then return the new message vector and the ANDing of all of the conditions.

```haskell
  in  (ws', and satVec)
```

With this new multi-step modification,
the total second-round modifications are only a little more complex.

```haskell
modifySecondRound :: V.Vector Word32 -> V.Vector Word32
modifySecondRound =
```

Step 17 is the easiest:

```haskell
  let funs17 = map (multiStepModification 16) (conditions V.! 16)
      modify17 = foldl (flip (.)) id funs17
```

The modifications for steps 18 and 19 are as for step 17,
just using the augmented modification function:

```haskell
      funs1819 = map (multiStepModification' 17) (conditions V.! 17)  ++
                 map (multiStepModification' 18) (conditions V.! 18)
```

We apply the modifications one at a time,
and check if the first round conditions are still satisfied.

```haskell
      modify1819 [] ws = ws
      modify1819 (f:fs) ws = case f ws of
```

If all are still satisfied, we can continue to the next function.

```haskell
        (ws', True) -> modify1819 fs ws'
```

Otherwise, we have to rerun all of the modifications.

```haskell
        (ws', False) -> modifySecondRound $ modifyFirstRound ws'
  in  modify1819 funs1819 . modify17
```
