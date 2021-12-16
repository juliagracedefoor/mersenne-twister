module System.Random.MT
  ( MTGen
  , new
  , fromSeed
  )
where

import           Control.Arrow                  ( Arrow((&&&)) )
import           Data.Bits                      ( Bits
                                                  ( complement
                                                  , shiftL
                                                  , shiftR
                                                  , xor
                                                  , (.&.)
                                                  , (.|.)
                                                  )
                                                )
import           Data.Sequence                  ( Seq((:<|), (:|>))
                                                , (<|)
                                                , (|>)
                                                )
import qualified Data.Sequence                 as Seq
import           Data.Word                      ( Word64 )
import           Numeric                        ( readHex )
import           System.Random                  ( RandomGen(genWord64, split)
                                                , getStdRandom
                                                , uniform
                                                )

-- let's define an interface

newtype MTGen = MTGen (Seq Word64)

instance RandomGen MTGen where
  genWord64 (MTGen xs) = (extract &&& MTGen) . twist $ xs
  split _ = error "i have no idea how this would be split"

instance Show MTGen where
  show (MTGen (x :<| _)) = "MTGen [" ++ show x ++ "...]"

new :: IO MTGen
new = fromSeed <$> getStdRandom uniform

fromSeed :: Word64 -> MTGen
fromSeed = MTGen . initialize

-- here's the computational stuff

-- word size (in bits)
w :: Int
w = 64

-- degree of recurrence
n :: Int
n = 312

-- middle word, an offset used to help define x (1 <= m < n)
m :: Int
m = 156

-- separation point of one word (0 <= r <= w - 1)
r :: Int
r = 31

-- coefficients of the rational form twist matrix
a :: Word64
[(a, _)] = readHex "B5026F5AA96619E9"

-- TGFSR(R) tempering bitmasks
b :: Word64
[(b, _)] = readHex "71D67FFFEDA60000"

c :: Word64
[(c, _)] = readHex "FFF7EEE000000000"

-- additional Mersenne Twister bitmask
d :: Word64
[(d, _)] = readHex "5555555555555555"

-- TGFSR(R) tempering bit shifts
s :: Int
s = 17

t :: Int
t = 37

-- additional Mersenne Twister bit shifts
u :: Int
u = 29

l :: Int
l = 43

-- an extra parameter for creating the initial state
f :: Word64
f = 6364136223846793005

-- 2 ^ (n * w - r) - 1 is a Mersenne prime

-- extract the random number that corresponds to the current state of the sequence
extract :: Seq Word64 -> Word64
extract (_ :|> x) = temper x

temper :: Word64 -> Word64
temper s0 = tempered
 where
  s1       = s0 `xor` ((s0 `shiftR` u) .&. d)
  s2       = s1 `xor` ((s1 `shiftL` s) .&. b)
  s3       = s2 `xor` ((s2 `shiftL` t) .&. c)
  tempered = s3 `xor` (s3 `shiftR` l)

-- update the sequence so that a new value can be extracted from the end of it
twist :: Seq Word64 -> Seq Word64
twist xs = Seq.drop 1 xs |> next
 where
  x0   = xs `Seq.index` 0
  x1   = xs `Seq.index` 1
  x2   = xs `Seq.index` m

  mask = (1 `shiftL` r) - 1
  applyA x = if even x then x `shiftR` 1 else (x `shiftR` 1) `xor` a

  upper = x0 .&. complement mask
  lower = x1 .&. mask
  next  = x2 `xor` applyA (upper .|. lower)

-- create a sequence of n values that are calculated based on the given seed
initialize :: Word64 -> Seq Word64
initialize = initialize' (n - 1)

initialize' :: Int -> Word64 -> Seq Word64
initialize' 0 seed = Seq.singleton seed
initialize' i seed = xs |> next
 where
  xs@(_ :|> prev) = initialize' (i - 1) seed
  next            = f * (prev `xor` (prev `shiftR` (w - 2))) + fromIntegral i
