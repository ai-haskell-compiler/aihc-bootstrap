module Data.Int (module Data.Int, module Prelude) where

-- The instances are the same for every built-in number type; the arithmetic
-- itself is in src/numbers.c, which goes by the name of the type.

data Int8

instance Eq Int8 where
  (==) = numEq

instance Ord Int8 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Int8 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Int8

instance Enum Int8 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Int8 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Int8 where
  showsPrec = showsNumber

instance Bounded Int8 where
  minBound = negate 128
  maxBound = 127

data Int16

instance Eq Int16 where
  (==) = numEq

instance Ord Int16 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Int16 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Int16

instance Enum Int16 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Int16 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Int16 where
  showsPrec = showsNumber

instance Bounded Int16 where
  minBound = negate 32768
  maxBound = 32767

data Int32

instance Eq Int32 where
  (==) = numEq

instance Ord Int32 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Int32 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Int32

instance Enum Int32 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Int32 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Int32 where
  showsPrec = showsNumber

instance Bounded Int32 where
  minBound = negate 2147483648
  maxBound = 2147483647

data Int64

instance Eq Int64 where
  (==) = numEq

instance Ord Int64 where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Int64 where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Int64

instance Enum Int64 where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Int64 where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Int64 where
  showsPrec = showsNumber

instance Bounded Int64 where
  minBound = negate 9223372036854775807 - 1
  maxBound = 9223372036854775807
