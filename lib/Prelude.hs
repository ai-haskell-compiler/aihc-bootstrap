module Prelude
  ( module Prelude,
    -- Defined elsewhere and passed on, so that a module that imports both
    -- this and the defining module sees one definition.
    readFile,
    writeFile,
    appendFile,
    IOError,
    ioError,
    userError,
    foldr1,
    foldl1,
    scanl,
    Read (..),
    ReadS,
    reads,
    read,
    rationalLiteral,
  )
where

-- The interpreter's own Prelude. It has what GHC's Prelude exports, as far
-- as AIHC needs it, and the classes and types the interpreter itself knows.

import Control.Exception (ErrorCall (..), throw)
import Data.Char (isSpace)
import System.IO (appendFile, hPutStr, hPutStrLn, readFile, stdout, writeFile)
import Data.List (foldl1, foldr1, scanl)
import Data.Ratio (rationalLiteral)
import GHC.Float ()
import System.IO.Error (IOError, ioError, userError)
import Text.Read (Read (..), ReadS, read, reads)

infixr 9 .

infixl 9 !!

infixr 8 ^, ^^, **

infixl 7 *, /, :%, `quot`, `rem`, `div`, `mod`

infixl 6 +, -

infixr 6 <>

infixr 5 ++

infix 4 ==, /=, <, <=, >=, >, `elem`, `notElem`

infixl 4 <$>, <$, <*>, *>, <*

infixr 3 &&

infixr 2 ||

infixl 1 >>, >>=

infixr 1 =<<

infixr 0 $, $!, `seq`

-- ---- Types ----

data Char

type String = [Char]

type FilePath = String

type ShowS = String -> String

-- Every integral type is a C long in the interpreter (src/numbers.c).
data Int

data Integer

data Word

-- The bits of an IEEE number (src/numbers.c). The instances are in
-- GHC.Float.
data Double

data Float

-- A fraction: numerator :% denominator, in lowest terms, with a positive
-- denominator. Data.Ratio makes them and has the instances.
data Ratio a = a :% a

type Rational = Ratio Integer

-- The interpreter builds these by position: False first, True second.
data Bool = False | True
  deriving (Eq, Ord, Show, Enum, Bounded)

data Ordering = LT | EQ | GT
  deriving (Eq, Ord, Show, Enum, Bounded)

data Maybe a = Nothing | Just a
  deriving (Eq, Ord, Show)

data Either a b = Left a | Right b
  deriving (Eq, Ord, Show)

-- ---- Functions ----

id :: a -> a
id a = a

const :: a -> b -> a
const a _ = a

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \a -> f (g a)

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

($) :: (a -> b) -> a -> b
($) f a = f a

($!) :: (a -> b) -> a -> b
($!) f a = seq a (f a)

foreign import prim "seq" seq :: a -> b -> b

asTypeOf :: a -> a -> a
asTypeOf a _ = a

until :: (a -> Bool) -> (a -> a) -> a -> a
until done step a = if done a then a else until done step (step a)

error :: String -> a
error message = throw (ErrorCall message)

errorWithoutStackTrace :: String -> a
errorWithoutStackTrace = error

undefined :: a
undefined = error "Prelude.undefined"

-- ---- Bool, Maybe, Either, tuples ----

not :: Bool -> Bool
not True = False
not False = True

(&&) :: Bool -> Bool -> Bool
(&&) True other = other
(&&) False _ = False

(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) False other = other

otherwise :: Bool
otherwise = True

maybe :: b -> (a -> b) -> Maybe a -> b
maybe fallback _ Nothing = fallback
maybe _ f (Just a) = f a

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

fst :: (a, b) -> a
fst (a, _) = a

snd :: (a, b) -> b
snd (_, b) = b

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b

-- ---- Eq and Ord ----

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  (==) a b = not (a /= b)
  (/=) a b = not (a == b)

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
  max, min :: a -> a -> a
  compare a b = if a == b then EQ else if a <= b then LT else GT
  (<) a b = case compare a b of
    LT -> True
    _ -> False
  (<=) a b = case compare a b of
    GT -> False
    _ -> True
  (>) a b = case compare a b of
    GT -> True
    _ -> False
  (>=) a b = case compare a b of
    LT -> False
    _ -> True
  max a b = if a <= b then b else a
  min a b = if a <= b then a else b

instance Eq () where
  (==) _ _ = True

instance Ord () where
  compare _ _ = EQ

instance (Eq a, Eq b) => Eq (a, b) where
  (==) (a, b) (c, d) = a == c && b == d

instance (Ord a, Ord b) => Ord (a, b) where
  compare (a, b) (c, d) = compare a c <> compare b d

instance (Eq a, Eq b, Eq c) => Eq (a, b, c) where
  (==) (a, b, c) (d, e, f) = a == d && b == e && c == f

instance (Ord a, Ord b, Ord c) => Ord (a, b, c) where
  compare (a, b, c) (d, e, f) = compare a d <> compare b e <> compare c f

instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d) where
  (==) (a, b, c, d) (e, f, g, h) = a == e && b == f && c == g && d == h

instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d) where
  compare (a, b, c, d) (e, f, g, h) =
    compare a e <> compare b f <> compare c g <> compare d h

instance Eq a => Eq [a] where
  (==) [] [] = True
  (==) (a : rest) (b : others) = a == b && rest == others
  (==) _ _ = False

instance Ord a => Ord [a] where
  compare [] [] = EQ
  compare [] (_ : _) = LT
  compare (_ : _) [] = GT
  compare (a : rest) (b : others) = compare a b <> compare rest others

-- ---- Numbers ----

class Num a where
  (+), (-), (*) :: a -> a -> a
  negate, abs, signum :: a -> a
  fromInteger :: Integer -> a
  (-) a b = a + negate b
  negate a = fromInteger 0 - a

class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  -- For the built-in whole number types.
  toRational a = numConvert a :% 1

class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  recip a = fromInteger 1 / a
  (/) a b = a * recip b

class Fractional a => Floating a where
  pi :: a
  exp, log, sqrt :: a -> a
  sin, cos, tan, asin, acos, atan, sinh, cosh, tanh :: a -> a
  (**), logBase :: a -> a -> a
  (**) a b = exp (log a * b)
  logBase base a = log a / log base

class (Real a, Fractional a) => RealFrac a where
  -- The whole part, towards zero, and what is left.
  properFraction :: Integral b => a -> (b, a)
  truncate, round, ceiling, floor :: Integral b => a -> b
  truncate a = fst (properFraction a)
  -- To the nearest whole number; to the even one if it is in the middle.
  round a =
    let (n, r) = properFraction a
        m = if r < fromInteger 0 then n - 1 else n + 1
     in case compare (abs r) (fromRational (1 :% 2)) of
          LT -> n
          EQ -> if even n then n else m
          GT -> m
  ceiling a = let (n, r) = properFraction a in if r > fromInteger 0 then n + 1 else n
  floor a = let (n, r) = properFraction a in if r < fromInteger 0 then n - 1 else n

class (RealFrac a, Floating a) => RealFloat a where
  floatRadix :: a -> Integer
  -- How many bits the mantissa has.
  floatDigits :: a -> Int
  floatRange :: a -> (Int, Int)
  -- The number is mantissa * 2^exponent.
  decodeFloat :: a -> (Integer, Int)
  encodeFloat :: Integer -> Int -> a
  isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE :: a -> Bool
  exponent :: a -> Int
  significand :: a -> a
  scaleFloat :: Int -> a -> a
  atan2 :: a -> a -> a
  floatRadix _ = 2
  isIEEE _ = True
  exponent a = case decodeFloat a of
    (m, n) -> if m == 0 then 0 else n + floatDigits a
  significand a = case decodeFloat a of
    (m, _) -> encodeFloat m (negate (floatDigits a))
  scaleFloat k a = case decodeFloat a of
    (m, n) -> if m == 0 then a else encodeFloat m (n + k)

class Enum a where
  succ, pred :: a -> a
  toEnum :: Int -> a
  fromEnum :: a -> Int
  enumFrom :: a -> [a]
  enumFromThen :: a -> a -> [a]
  enumFromTo :: a -> a -> [a]
  enumFromThenTo :: a -> a -> a -> [a]
  succ a = toEnum (fromEnum a + 1)
  pred a = toEnum (fromEnum a - 1)
  enumFrom a = map toEnum (enumFrom (fromEnum a))
  enumFromThen a b = map toEnum (enumFromThen (fromEnum a) (fromEnum b))
  enumFromTo a b = map toEnum (enumFromTo (fromEnum a) (fromEnum b))
  enumFromThenTo a b c =
    map toEnum (enumFromThenTo (fromEnum a) (fromEnum b) (fromEnum c))

class Bounded a where
  minBound, maxBound :: a

class (Real a, Enum a) => Integral a where
  quot, rem, div, mod :: a -> a -> a
  quotRem, divMod :: a -> a -> (a, a)
  toInteger :: a -> Integer
  quotRem a b = (quot a b, rem a b)
  divMod a b = (div a b, mod a b)

-- The arithmetic of every built-in number type. Which type it is decides
-- how the result wraps around.
foreign import prim "numAdd" numAdd :: a -> a -> a

foreign import prim "numSub" numSub :: a -> a -> a

foreign import prim "numMul" numMul :: a -> a -> a

foreign import prim "numQuot" numQuot :: a -> a -> a

foreign import prim "numRem" numRem :: a -> a -> a

foreign import prim "numDiv" numDiv :: a -> a -> a

foreign import prim "numMod" numMod :: a -> a -> a

foreign import prim "numEq" numEq :: a -> a -> Bool

foreign import prim "numLess" numLess :: a -> a -> Bool

-- From one built-in number type to another, or to and from Char.
foreign import prim "numConvert" numConvert :: a -> b

-- The digits of a number, with a minus sign if it is negative.
foreign import prim "numShow" numShow :: a -> String

numberAbs :: (Ord a, Num a) => a -> a
numberAbs n = if n < 0 then negate n else n

numberSignum :: (Ord a, Num a) => a -> a
numberSignum n = if n < 0 then negate 1 else if n == 0 then 0 else 1

numberEnumFrom :: Num a => a -> [a]
numberEnumFrom n = n : numberEnumFrom (n + 1)

numberEnumFromThen :: Num a => a -> a -> [a]
numberEnumFromThen a b = a : numberEnumFromThen b (b + b - a)

numberEnumFromTo :: (Ord a, Num a) => a -> a -> [a]
numberEnumFromTo a b = if a > b then [] else a : (if a == b then [] else numberEnumFromTo (a + 1) b)

numberEnumFromThenTo :: (Ord a, Num a) => a -> a -> a -> [a]
numberEnumFromThenTo a b limit
  | b >= a = if a > limit then [] else a : up b
  | otherwise = if a < limit then [] else a : down b
  where
    step = b - a
    -- The next element is not computed until it is known to be in range,
    -- so that the last step cannot wrap around.
    up n = if n > limit then [] else n : (if limit - n < step then [] else up (n + step))
    down n = if n < limit then [] else n : (if limit - n > step then [] else down (n + step))

boundedEnumFrom :: (Ord a, Num a, Bounded a) => a -> [a]
boundedEnumFrom n = numberEnumFromTo n maxBound

boundedEnumFromThen :: (Ord a, Num a, Bounded a) => a -> a -> [a]
boundedEnumFromThen a b = numberEnumFromThenTo a b (if b >= a then maxBound else minBound)

showsNumber :: (Ord a, Num a) => Int -> a -> ShowS
showsNumber precedence n rest =
  if n < 0 && precedence > 6
    then '(' : numShow n ++ (')' : rest)
    else numShow n ++ rest

instance Eq Int where
  (==) = numEq

instance Ord Int where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Int where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Int

instance Enum Int where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Int where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Int where
  showsPrec = showsNumber

instance Bounded Int where
  minBound = negate 9223372036854775807 - 1
  maxBound = 9223372036854775807

instance Eq Integer where
  (==) = numEq

instance Ord Integer where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Integer where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Integer

instance Enum Integer where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = numberEnumFrom
  enumFromThen = numberEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Integer where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Integer where
  showsPrec = showsNumber

instance Eq Word where
  (==) = numEq

instance Ord Word where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Num Word where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  abs = numberAbs
  signum = numberSignum
  fromInteger = numConvert

instance Real Word

instance Enum Word where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  enumFromTo = numberEnumFromTo
  enumFromThenTo = numberEnumFromThenTo

instance Integral Word where
  quot = numQuot
  rem = numRem
  div = numDiv
  mod = numMod
  toInteger = numConvert

instance Show Word where
  showsPrec = showsNumber

instance Bounded Word where
  minBound = 0
  maxBound = 18446744073709551615

subtract :: Num a => a -> a -> a
subtract a b = b - a

even :: Integral a => a -> Bool
even n = rem n 2 == 0

odd :: Integral a => a -> Bool
odd n = not (even n)

gcd :: Integral a => a -> a -> a
gcd a b = if b == 0 then abs a else gcd b (rem a b)

lcm :: Integral a => a -> a -> a
lcm a b = if a == 0 || b == 0 then 0 else abs (quot a (gcd a b) * b)

(^) :: (Num a, Integral b) => a -> b -> a
(^) base exponent
  | exponent < 0 = error "Negative exponent"
  | exponent == 0 = 1
  | even exponent = half * half
  | otherwise = base * base ^ (exponent - 1)
  where
    half = base ^ quot exponent 2

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral a = fromInteger (toInteger a)

realToFrac :: (Real a, Fractional b) => a -> b
realToFrac a = fromRational (toRational a)

(^^) :: (Fractional a, Integral b) => a -> b -> a
(^^) base n = if n >= 0 then base ^ n else recip (base ^ negate n)

-- ---- Char ----

instance Eq Char where
  (==) = numEq

instance Ord Char where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess

instance Enum Char where
  toEnum = numConvert
  fromEnum = numConvert
  enumFrom c = enumFromTo c maxBound
  enumFromThen a b = enumFromThenTo a b (if b >= a then maxBound else minBound)
  enumFromTo a b = map toEnum (enumFromTo (fromEnum a) (fromEnum b))
  enumFromThenTo a b c =
    map toEnum (enumFromThenTo (fromEnum a) (fromEnum b) (fromEnum c))

instance Bounded Char where
  minBound = '\0'
  maxBound = '\1114111'

-- ---- Show ----

class Show a where
  showsPrec :: Int -> a -> ShowS
  show :: a -> String
  showList :: [a] -> ShowS
  showsPrec _ a rest = show a ++ rest
  show a = showsPrec 0 a ""
  showList list rest = showListWith shows list rest

shows :: Show a => a -> ShowS
shows = showsPrec 0

showChar :: Char -> ShowS
showChar c rest = c : rest

showString :: String -> ShowS
showString text rest = text ++ rest

showParen :: Bool -> ShowS -> ShowS
showParen wanted inner rest = if wanted then '(' : inner (')' : rest) else inner rest

showListWith :: (a -> ShowS) -> [a] -> ShowS
showListWith _ [] rest = "[]" ++ rest
showListWith showItem (first : others) rest = '[' : showItem first (go others)
  where
    go [] = ']' : rest
    go (item : more) = ',' : showItem item (go more)

instance Show () where
  showsPrec _ () rest = "()" ++ rest

instance Show a => Show [a] where
  showsPrec _ = showList

instance (Show a, Show b) => Show (a, b) where
  showsPrec _ (a, b) rest = '(' : shows a (',' : shows b (')' : rest))

instance (Show a, Show b, Show c) => Show (a, b, c) where
  showsPrec _ (a, b, c) rest = '(' : shows a (',' : shows b (',' : shows c (')' : rest)))

instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d) where
  showsPrec _ (a, b, c, d) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (')' : rest))))

instance Show Char where
  showsPrec _ '\'' rest = "'\\''" ++ rest
  showsPrec _ c rest = '\'' : showLitChar c ('\'' : rest)
  showList text rest = '"' : showLitString text ('"' : rest)

showLitString :: String -> ShowS
showLitString [] rest = rest
showLitString ('"' : text) rest = "\\\"" ++ showLitString text rest
showLitString (c : text) rest = showLitChar c (showLitString text rest)

-- A character as it is written inside a literal. An escape that ends in
-- digits, or \SO, gets \& after it if what follows would change its meaning.
showLitChar :: Char -> ShowS
showLitChar c rest
  | c > '\DEL' = '\\' : protectEscape isDigitChar (numShow (fromEnum c)) rest
  | c == '\DEL' = "\\DEL" ++ rest
  | c == '\\' = "\\\\" ++ rest
  | c >= ' ' = c : rest
  | c == '\a' = "\\a" ++ rest
  | c == '\b' = "\\b" ++ rest
  | c == '\f' = "\\f" ++ rest
  | c == '\n' = "\\n" ++ rest
  | c == '\r' = "\\r" ++ rest
  | c == '\t' = "\\t" ++ rest
  | c == '\v' = "\\v" ++ rest
  | c == '\SO' = '\\' : protectEscape (== 'H') "SO" rest
  | otherwise = '\\' : (controlNames !! fromEnum c ++ rest)

protectEscape :: (Char -> Bool) -> String -> ShowS
protectEscape clashes escape rest = escape ++ protect rest
  where
    protect text@(c : _) = if clashes c then "\\&" ++ text else text
    protect [] = []

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

controlNames :: [String]
controlNames =
  [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL", "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI",
    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB", "CAN", "EM", "SUB", "ESC", "FS", "GS", "RS", "US"
  ]

-- Larger tuples, up to the 15 elements GHC has instances for. Their Eq and
-- Ord instances are here too, since Show is the last of the three classes
-- to be defined.

instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e) where
  (==) (a, b, c, d, e) (a2, b2, c2, d2, e2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2

instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e) where
  compare (a, b, c, d, e) (a2, b2, c2, d2, e2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2

instance (Show a, Show b, Show c, Show d, Show e) => Show (a, b, c, d, e) where
  showsPrec _ (a, b, c, d, e) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (')' : rest)))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (a, b, c, d, e, f) where
  (==) (a, b, c, d, e, f) (a2, b2, c2, d2, e2, f2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) => Ord (a, b, c, d, e, f) where
  compare (a, b, c, d, e, f) (a2, b2, c2, d2, e2, f2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2

instance (Show a, Show b, Show c, Show d, Show e, Show f) => Show (a, b, c, d, e, f) where
  showsPrec _ (a, b, c, d, e, f) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (')' : rest))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (a, b, c, d, e, f, g) where
  (==) (a, b, c, d, e, f, g) (a2, b2, c2, d2, e2, f2, g2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) => Ord (a, b, c, d, e, f, g) where
  compare (a, b, c, d, e, f, g) (a2, b2, c2, d2, e2, f2, g2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (a, b, c, d, e, f, g) where
  showsPrec _ (a, b, c, d, e, f, g) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (')' : rest)))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (a, b, c, d, e, f, g, h) where
  (==) (a, b, c, d, e, f, g, h) (a2, b2, c2, d2, e2, f2, g2, h2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h) => Ord (a, b, c, d, e, f, g, h) where
  compare (a, b, c, d, e, f, g, h) (a2, b2, c2, d2, e2, f2, g2, h2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (a, b, c, d, e, f, g, h) where
  showsPrec _ (a, b, c, d, e, f, g, h) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (')' : rest))))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (a, b, c, d, e, f, g, h, i) where
  (==) (a, b, c, d, e, f, g, h, i) (a2, b2, c2, d2, e2, f2, g2, h2, i2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2 && i == i2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i) => Ord (a, b, c, d, e, f, g, h, i) where
  compare (a, b, c, d, e, f, g, h, i) (a2, b2, c2, d2, e2, f2, g2, h2, i2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2 <> compare i i2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (a, b, c, d, e, f, g, h, i) where
  showsPrec _ (a, b, c, d, e, f, g, h, i) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (',' : shows i (')' : rest)))))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (a, b, c, d, e, f, g, h, i, j) where
  (==) (a, b, c, d, e, f, g, h, i, j) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2 && i == i2 && j == j2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j) => Ord (a, b, c, d, e, f, g, h, i, j) where
  compare (a, b, c, d, e, f, g, h, i, j) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2 <> compare i i2 <> compare j j2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (a, b, c, d, e, f, g, h, i, j) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (',' : shows i (',' : shows j (')' : rest))))))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k) => Eq (a, b, c, d, e, f, g, h, i, j, k) where
  (==) (a, b, c, d, e, f, g, h, i, j, k) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2 && i == i2 && j == j2 && k == k2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k) => Ord (a, b, c, d, e, f, g, h, i, j, k) where
  compare (a, b, c, d, e, f, g, h, i, j, k) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2 <> compare i i2 <> compare j j2 <> compare k k2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k) => Show (a, b, c, d, e, f, g, h, i, j, k) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j, k) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (',' : shows i (',' : shows j (',' : shows k (')' : rest)))))))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l) => Eq (a, b, c, d, e, f, g, h, i, j, k, l) where
  (==) (a, b, c, d, e, f, g, h, i, j, k, l) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2 && i == i2 && j == j2 && k == k2 && l == l2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l) => Ord (a, b, c, d, e, f, g, h, i, j, k, l) where
  compare (a, b, c, d, e, f, g, h, i, j, k, l) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2 <> compare i i2 <> compare j j2 <> compare k k2 <> compare l l2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l) => Show (a, b, c, d, e, f, g, h, i, j, k, l) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j, k, l) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (',' : shows i (',' : shows j (',' : shows k (',' : shows l (')' : rest))))))))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  (==) (a, b, c, d, e, f, g, h, i, j, k, l, m) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2 && i == i2 && j == j2 && k == k2 && l == l2 && m == m2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  compare (a, b, c, d, e, f, g, h, i, j, k, l, m) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2 <> compare i i2 <> compare j j2 <> compare k k2 <> compare l l2 <> compare m m2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j, k, l, m) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (',' : shows i (',' : shows j (',' : shows k (',' : shows l (',' : shows m (')' : rest)))))))))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  (==) (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2 && i == i2 && j == j2 && k == k2 && l == l2 && m == m2 && n == n2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  compare (a, b, c, d, e, f, g, h, i, j, k, l, m, n) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2 <> compare i i2 <> compare j j2 <> compare k k2 <> compare l l2 <> compare m m2 <> compare n n2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (',' : shows i (',' : shows j (',' : shows k (',' : shows l (',' : shows m (',' : shows n (')' : rest))))))))))))))

instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  (==) (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2) =
    a == a2 && b == b2 && c == c2 && d == d2 && e == e2 && f == f2 && g == g2 && h == h2 && i == i2 && j == j2 && k == k2 && l == l2 && m == m2 && n == n2 && o == o2

instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h, Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o) => Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  compare (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2) =
    compare a a2 <> compare b b2 <> compare c c2 <> compare d d2 <> compare e e2 <> compare f f2 <> compare g g2 <> compare h h2 <> compare i i2 <> compare j j2 <> compare k k2 <> compare l l2 <> compare m m2 <> compare n n2 <> compare o o2

instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  showsPrec _ (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) rest =
    '(' : shows a (',' : shows b (',' : shows c (',' : shows d (',' : shows e (',' : shows f (',' : shows g (',' : shows h (',' : shows i (',' : shows j (',' : shows k (',' : shows l (',' : shows m (',' : shows n (',' : shows o (')' : rest)))))))))))))))

-- ---- Semigroup and Monoid ----

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mappend = (<>)
  mconcat = foldr mappend mempty

instance Semigroup [a] where
  (<>) = (++)

instance Monoid [a] where
  mempty = []

instance Semigroup Ordering where
  (<>) LT _ = LT
  (<>) EQ other = other
  (<>) GT _ = GT

instance Monoid Ordering where
  mempty = EQ

instance Semigroup () where
  (<>) _ _ = ()

instance Monoid () where
  mempty = ()

instance Semigroup a => Semigroup (Maybe a) where
  (<>) Nothing other = other
  (<>) first Nothing = first
  (<>) (Just a) (Just b) = Just (a <> b)

instance Semigroup a => Monoid (Maybe a) where
  mempty = Nothing

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (<>) (a, b) (c, d) = (a <> c, b <> d)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  mempty = (mempty, mempty)

instance Semigroup b => Semigroup (a -> b) where
  (<>) f g = \a -> f a <> g a

instance Monoid b => Monoid (a -> b) where
  mempty = \_ -> mempty

-- ---- Functor, Applicative, Monad ----

class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) a = fmap (\_ -> a)

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
  (<*>) = liftA2 id
  liftA2 f a b = fmap f a <*> b
  (*>) a b = (id <$ a) <*> b
  (<*) = liftA2 const

class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
  (>>) first second = first >>= (\_ -> second)
  return = pure

class Monad m => MonadFail m where
  fail :: String -> m a

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) f action = action >>= f

instance Functor [] where
  fmap = map

instance Applicative [] where
  pure a = [a]
  (<*>) functions list = concatMap (\f -> map f list) functions

instance Monad [] where
  (>>=) list f = concatMap f list

instance MonadFail [] where
  fail _ = []

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)

instance Applicative Maybe where
  pure = Just
  (<*>) (Just f) (Just a) = Just (f a)
  (<*>) _ _ = Nothing

instance Monad Maybe where
  (>>=) Nothing _ = Nothing
  (>>=) (Just a) f = f a

instance MonadFail Maybe where
  fail _ = Nothing

instance Functor (Either e) where
  fmap _ (Left e) = Left e
  fmap f (Right a) = Right (f a)

instance Applicative (Either e) where
  pure = Right
  (<*>) (Left e) _ = Left e
  (<*>) (Right f) other = fmap f other

instance Monad (Either e) where
  (>>=) (Left e) _ = Left e
  (>>=) (Right a) f = f a

instance Functor ((->) r) where
  fmap = (.)

instance Applicative ((->) r) where
  pure = const
  (<*>) f g = \r -> f r (g r)

instance Monad ((->) r) where
  (>>=) f k = \r -> k (f r) r

-- ---- IO ----

-- An IO action is a function from a world token to an IOResult. The
-- interpreter knows this representation (src/eval.h).
data World

data IOResult a = IOResult a

newtype IO a = IO (World -> IOResult a)

returnIO :: a -> IO a
returnIO a = IO (\world -> IOResult a)

bindIO :: IO a -> (a -> IO b) -> IO b
bindIO (IO action) next = IO (\world -> case action world of
  IOResult a -> case next a of
    IO continue -> continue world)

instance Functor IO where
  fmap f action = bindIO action (\a -> returnIO (f a))

instance Applicative IO where
  pure = returnIO
  (<*>) functions arguments =
    bindIO functions (\f -> bindIO arguments (\a -> returnIO (f a)))

instance Monad IO where
  (>>=) = bindIO

instance MonadFail IO where
  fail message = error message

putStr :: String -> IO ()
putStr = hPutStr stdout

putStrLn :: String -> IO ()
putStrLn = hPutStrLn stdout

print :: Show a => a -> IO ()
print a = putStrLn (show a)

-- ---- Foldable and Traversable ----

-- foldMap and foldr are each defined by the other, so an instance gives
-- one of them. The other methods of GHC's class are plain functions here.
class Foldable t where
  foldMap :: Monoid m => (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldMap f = foldr (\a rest -> f a <> rest) mempty
  foldr f end container = applyFoldEndo (foldMap (\a -> FoldEndo (f a)) container) end

-- Functions from a type to itself, composed. Data.Monoid has the Endo that
-- programs use; this one is only for the default of foldr.
newtype FoldEndo a = FoldEndo (a -> a)

applyFoldEndo :: FoldEndo a -> a -> a
applyFoldEndo (FoldEndo f) = f

instance Semigroup (FoldEndo a) where
  (<>) (FoldEndo f) (FoldEndo g) = FoldEndo (\a -> f (g a))

instance Monoid (FoldEndo a) where
  mempty = FoldEndo id

instance Foldable [] where
  foldr _ end [] = end
  foldr f end (a : rest) = f a (foldr f end rest)

instance Foldable Maybe where
  foldr _ end Nothing = end
  foldr f end (Just a) = f a end

foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl f start container = foldr (\a continue total -> continue (f total a)) id container start

foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b
foldl' f start container = foldr (\a continue total -> continue $! f total a) id container start

toList :: Foldable t => t a -> [a]
toList = foldr (:) []

null :: Foldable t => t a -> Bool
null = foldr (\_ _ -> False) True

length :: Foldable t => t a -> Int
length = foldl' (\count _ -> count + 1) 0

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem a = any (== a)

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem a container = not (elem a container)

sum :: (Foldable t, Num a) => t a -> a
sum = foldl' (+) 0

product :: (Foldable t, Num a) => t a -> a
product = foldl' (*) 1

maximum :: (Foldable t, Ord a) => t a -> a
maximum container = case toList container of
  [] -> error "maximum: empty structure"
  first : rest -> foldl' max first rest

minimum :: (Foldable t, Ord a) => t a -> a
minimum container = case toList container of
  [] -> error "minimum: empty structure"
  first : rest -> foldl' min first rest

any :: Foldable t => (a -> Bool) -> t a -> Bool
any test = foldr (\a rest -> test a || rest) False

all :: Foldable t => (a -> Bool) -> t a -> Bool
all test = foldr (\a rest -> test a && rest) True

and :: Foldable t => t Bool -> Bool
and = all id

or :: Foldable t => t Bool -> Bool
or = any id

concat :: Foldable t => t [a] -> [a]
concat = foldr (++) []

concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f = foldr (\a rest -> f a ++ rest) []

mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f = foldr (\a rest -> f a >> rest) (pure ())

sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (\action rest -> action >> rest) (pure ())

traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (\a rest -> f a *> rest) (pure ())

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM :: Monad m => (a -> m b) -> t a -> m (t b)
  sequence :: Monad m => t (m a) -> m (t a)
  traverse f container = sequenceA (fmap f container)
  sequenceA = traverse id
  mapM = traverse
  sequence = sequenceA

instance Traversable [] where
  traverse _ [] = pure []
  traverse f (a : rest) = liftA2 (:) (f a) (traverse f rest)

-- A pair holds its second part.
instance Functor ((,) c) where
  fmap f (c, a) = (c, f a)

instance Foldable ((,) c) where
  foldr f end (_, a) = f a end

instance Traversable ((,) c) where
  traverse f (c, a) = fmap (\b -> (c, b)) (f a)

instance Foldable (Either e) where
  foldr _ end (Left _) = end
  foldr f end (Right a) = f a end

instance Traversable (Either e) where
  traverse _ (Left e) = pure (Left e)
  traverse f (Right a) = fmap Right (f a)

instance Traversable Maybe where
  traverse _ Nothing = pure Nothing
  traverse f (Just a) = fmap Just (f a)

-- ---- Lists ----

(++) :: [a] -> [a] -> [a]
(++) [] other = other
(++) (first : rest) other = first : (rest ++ other)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a : rest) = f a : map f rest

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter keep (a : rest)
  | keep a = a : filter keep rest
  | otherwise = filter keep rest

head :: [a] -> a
head (a : _) = a
head [] = error "Prelude.head: empty list"

last :: [a] -> a
last [a] = a
last (_ : rest) = last rest
last [] = error "Prelude.last: empty list"

tail :: [a] -> [a]
tail (_ : rest) = rest
tail [] = error "Prelude.tail: empty list"

init :: [a] -> [a]
init [_] = []
init (a : rest) = a : init rest
init [] = error "Prelude.init: empty list"

(!!) :: [a] -> Int -> a
(!!) [] _ = error "Prelude.!!: index too large"
(!!) (a : rest) n = if n == 0 then a else rest !! (n - 1)

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

take :: Int -> [a] -> [a]
take _ [] = []
take n (a : rest) = if n <= 0 then [] else a : take (n - 1) rest

drop :: Int -> [a] -> [a]
drop _ [] = []
drop n list@(_ : rest) = if n <= 0 then list else drop (n - 1) rest

splitAt :: Int -> [a] -> ([a], [a])
splitAt n list = (take n list, drop n list)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile keep (a : rest) = if keep a then a : takeWhile keep rest else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile skip list@(a : rest) = if skip a then dropWhile skip rest else list

span :: (a -> Bool) -> [a] -> ([a], [a])
span _ [] = ([], [])
span keep list@(a : rest) =
  if keep a
    then case span keep rest of
      (front, back) -> (a : front, back)
    else ([], list)

break :: (a -> Bool) -> [a] -> ([a], [a])
break stop = span (not . stop)

replicate :: Int -> a -> [a]
replicate n a = if n <= 0 then [] else a : replicate (n - 1) a

iterate :: (a -> a) -> a -> [a]
iterate f a = a : iterate f (f a)

repeat :: a -> [a]
repeat a = a : repeat a

cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle list = list ++ cycle list

zip :: [a] -> [b] -> [(a, b)]
zip = zipWith (\a b -> (a, b))

zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (a : as) (b : bs) (c : cs) = (a, b, c) : zip3 as bs cs
zip3 _ _ _ = []

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a : as) (b : bs) = f a b : zipWith f as bs
zipWith _ _ _ = []

zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 f (a : as) (b : bs) (c : cs) = f a b c : zipWith3 f as bs cs
zipWith3 _ _ _ _ = []

unzip :: [(a, b)] -> ([a], [b])
unzip pairs = (map fst pairs, map snd pairs)

unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 [] = ([], [], [])
unzip3 ((a, b, c) : rest) = case unzip3 rest of
  (as, bs, cs) -> (a : as, b : bs, c : cs)

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _ [] = Nothing
lookup key ((candidate, value) : rest) = if key == candidate then Just value else lookup key rest

lines :: String -> [String]
lines "" = []
lines text = case break (== '\n') text of
  (line, rest) -> line : case rest of
    [] -> []
    _ : more -> lines more

unlines :: [String] -> String
unlines = concatMap (++ "\n")

words :: String -> [String]
words text = case dropWhile isSpace text of
  "" -> []
  rest -> case break isSpace rest of
    (word, more) -> word : words more

unwords :: [String] -> String
unwords [] = ""
unwords [word] = word
unwords (word : rest) = word ++ (' ' : unwords rest)
