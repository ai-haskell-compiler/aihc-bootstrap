module Data.Bits where

-- Bit operations on the built-in number types. The work is done by
-- src/numbers.c, which goes by the type.

import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

infixl 8 `shift`, `shiftL`, `shiftR`

infixl 7 .&.

infixl 6 `xor`

infixl 5 .|.

class Eq a => Bits a where
  (.&.), (.|.), xor :: a -> a -> a
  complement :: a -> a
  shiftL, shiftR, shift :: a -> Int -> a
  zeroBits :: a
  bit :: Int -> a
  testBit :: a -> Int -> Bool
  setBit, clearBit :: a -> Int -> a
  popCount :: a -> Int
  (.&.) = numAnd
  (.|.) = numOr
  xor = numXor
  complement = numComplement
  shiftL = numShiftL
  shiftR = numShiftR
  shift a n = if n >= 0 then shiftL a n else shiftR a (negate n)
  zeroBits = numConvert (0 :: Int)
  bit n = shiftL (numConvert (1 :: Int)) n
  testBit a n = (a .&. bit n) /= zeroBits
  setBit a n = a .|. bit n
  clearBit a n = a .&. complement (bit n)
  popCount a = countBits a

class Bits a => FiniteBits a where
  finiteBitSize :: a -> Int
  countLeadingZeros :: a -> Int
  countTrailingZeros :: a -> Int
  countLeadingZeros a = leadingZeros (finiteBitSize a - 1) a
  countTrailingZeros a = trailingZeros (finiteBitSize a) 0 a

-- How many bits are set, for a value of a width up to 64 bits.
countBits :: Bits a => a -> Int
countBits a = go 0 0
  where
    go n total = if n == 64 then total else go (n + 1) (if testBit a n then total + 1 else total)

leadingZeros :: Bits a => Int -> a -> Int
leadingZeros top a = go top 0
  where
    go n total = if n < 0 || testBit a n then total else go (n - 1) (total + 1)

trailingZeros :: Bits a => Int -> Int -> a -> Int
trailingZeros size n a = if n == size || testBit a n then n else trailingZeros size (n + 1) a

foreign import prim "numAnd" numAnd :: a -> a -> a

foreign import prim "numOr" numOr :: a -> a -> a

foreign import prim "numXor" numXor :: a -> a -> a

foreign import prim "numComplement" numComplement :: a -> a

foreign import prim "numShiftL" numShiftL :: a -> Int -> a

foreign import prim "numShiftR" numShiftR :: a -> Int -> a

instance Bits Int

instance FiniteBits Int where
  finiteBitSize _ = 64

instance Bits Integer where
  popCount a = if a < 0 then negate (integerOnes (negate a)) else integerOnes a

-- How many bits of a number that is not negative are 1.
integerOnes :: Integer -> Int
integerOnes n = if n == 0 then 0 else fromInteger (n `rem` 2) + integerOnes (n `quot` 2)

instance Bits Word

instance FiniteBits Word where
  finiteBitSize _ = 64

instance Bits Word8

instance FiniteBits Word8 where
  finiteBitSize _ = 8

instance Bits Word16

instance FiniteBits Word16 where
  finiteBitSize _ = 16

instance Bits Word32

instance FiniteBits Word32 where
  finiteBitSize _ = 32

instance Bits Word64

instance FiniteBits Word64 where
  finiteBitSize _ = 64

instance Bits Int8

instance FiniteBits Int8 where
  finiteBitSize _ = 8

instance Bits Int16

instance FiniteBits Int16 where
  finiteBitSize _ = 16

instance Bits Int32

instance FiniteBits Int32 where
  finiteBitSize _ = 32

instance Bits Int64

instance FiniteBits Int64 where
  finiteBitSize _ = 64

instance Bits Bool where
  (.&.) = (&&)
  (.|.) = (||)
  xor a b = a /= b
  complement = not
  shiftL a n = if n == 0 then a else False
  shiftR a n = if n == 0 then a else False
  zeroBits = False
  bit n = n == 0
  testBit a n = a && n == 0
  popCount a = if a then 1 else 0
