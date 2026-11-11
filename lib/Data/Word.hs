module Data.Word (module Data.Word, module Prelude) where

-- The instances are the same for every built-in number type; the arithmetic
-- itself is in src/numbers.c, which goes by the name of the type.

data Word8

instance Eq Word8 where
  (==) = numEq

instance Ord Word8 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Word8 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Word8

instance Enum Word8 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Word8 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Word8 where
  showsPrec = showsNumber

instance Bounded Word8 where
  minBound = 0
  maxBound = 255

data Word16

instance Eq Word16 where
  (==) = numEq

instance Ord Word16 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Word16 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Word16

instance Enum Word16 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Word16 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Word16 where
  showsPrec = showsNumber

instance Bounded Word16 where
  minBound = 0
  maxBound = 65535

data Word32

instance Eq Word32 where
  (==) = numEq

instance Ord Word32 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Word32 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Word32

instance Enum Word32 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Word32 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Word32 where
  showsPrec = showsNumber

instance Bounded Word32 where
  minBound = 0
  maxBound = 4294967295

data Word64

instance Eq Word64 where
  (==) = numEq

instance Ord Word64 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Word64 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Word64

instance Enum Word64 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Word64 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Word64 where
  showsPrec = showsNumber

instance Bounded Word64 where
  minBound = 0
  maxBound = 18446744073709551615
