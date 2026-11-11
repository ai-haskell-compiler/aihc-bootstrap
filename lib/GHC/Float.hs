module GHC.Float where

-- Double and Float. The arithmetic is C's (src/numbers.c). What has to be
-- exact and C does not promise is done here with Integer arithmetic:
-- converting a fraction to the nearest number (fromRat), and finding the
-- shortest digits that still mean the same number (floatToDigits), which
-- is what `show` prints. Both follow the definitions in GHC's base.

import Data.Char (intToDigit, isDigit)
import Data.Int
import Data.Ratio (denominator, numerator, rationalLiteral, (%))
import Data.Word
import Text.Read (Read (..), expect)

-- ---- From a fraction ----

-- The number nearest to the fraction; the one with an even mantissa if two
-- are as near.
fromRat :: RealFloat a => Rational -> a
fromRat x
  | denominator x == 0 = encodeFloat 0 0 / encodeFloat 0 0
  | numerator x == 0 = encodeFloat 0 0
  | numerator x < 0 = negate (fromPositiveRat (negate x))
  | otherwise = fromPositiveRat x

-- Scale the fraction by a power of two until its whole part has as many
-- bits as a mantissa (or the exponent is as small as it gets), round it
-- once, and that is the mantissa.
fromPositiveRat :: RealFloat a => Rational -> a
fromPositiveRat x = result
  where
    digits = floatDigits result
    minExp = fst (floatRange result) - digits
    xMin = (2 ^ (digits - 1)) :% 1
    xMax = (2 ^ digits) :% 1
    p0 = max minExp (integerLog2 (numerator x) - integerLog2 (denominator x) - digits)
    factor = if p0 < 0 then 1 :% (2 ^ negate p0) else (2 ^ p0) :% 1
    x0 = x / factor
    (scaled, power) = if x0 >= xMin then scaleRat minExp xMin xMax p0 x0 else (x0, p0)
    result = encodeFloat (round scaled) power

scaleRat :: Int -> Rational -> Rational -> Int -> Rational -> (Rational, Int)
scaleRat minExp xMin xMax power x
  | power <= minExp = (x, power)
  | x >= xMax = scaleRat minExp xMin xMax (power + 1) (x / 2)
  | x < xMin = scaleRat minExp xMin xMax (power - 1) (x * 2)
  | otherwise = (x, power)

-- How many times a positive number can be halved before it is 1.
integerLog2 :: Integer -> Int
integerLog2 n = if n <= 1 then 0 else 1 + integerLog2 (quot n 2)

-- ---- To a fraction, to whole numbers ----

floatToRational :: RealFloat a => a -> Rational
floatToRational x = case decodeFloat x of
  (m, e) -> if e >= 0 then (m * 2 ^ e) :% 1 else m % (2 ^ negate e)

floatProperFraction :: (RealFloat a, Integral b) => a -> (b, a)
floatProperFraction x = case decodeFloat x of
  (m, e) ->
    if e >= 0
      then (fromInteger (m * 2 ^ e), encodeFloat 0 0)
      else let whole = quot m (2 ^ negate e) in (fromInteger whole, x - fromInteger whole)

-- A mantissa of any size: bits beyond what C can take are dropped.
encodeBig :: (Int -> Int -> a) -> Integer -> Int -> a
encodeBig encode m e =
  if abs m < 4611686018427387904
    then encode (fromInteger m) e
    else encodeBig encode (quot m 2) (e + 1)

-- ---- Shortest digits ----

-- The digits of a positive number and where the point goes: digits
-- d1 d2 ... and e stand for 0.d1d2... * 10^e. They are the fewest digits
-- that no other number of the type is nearer to (Burger and Dybvig,
-- "Printing Floating-Point Numbers Quickly and Accurately", as in GHC).
floatToDigits :: RealFloat a => a -> ([Int], Int)
floatToDigits x = case decodeFloat x of
  (f0, e0) ->
    if f0 == 0
      then ([0], 0)
      else
        let p = floatDigits x
            minExp = fst (floatRange x) - p
            -- A number too small for a full mantissa has its mantissa
            -- cut down to the bits it really has.
            n = minExp - e0
            f = if n > 0 then quot f0 (2 ^ n) else f0
            e = if n > 0 then e0 + n else e0
            -- r / s is the number; mUp / s and mDn / s are half the
            -- distance to the next number up and down.
            (r, s, mUp, mDn) =
              if e >= 0
                then
                  let be = 2 ^ e
                   in if f == 2 ^ (p - 1) then (f * be * 4, 4, be * 2, be) else (f * be * 2, 2, be, be)
                else
                  if e > minExp && f == 2 ^ (p - 1)
                    then (f * 4, 2 ^ (negate e + 1) * 2, 2, 1)
                    else (f * 2, 2 ^ negate e * 2, 1, 1)
            -- An estimate of the exponent, from the number of bits: 8651 /
            -- 28738 is just above the logarithm of 2 to the base 10.
            lx = p - 1 + e0
            k1 = quot (lx * 8651) 28738
            k0 = if lx >= 0 then k1 + 1 else k1
            fixup k =
              if k >= 0
                then if r + mUp <= 10 ^ k * s then k else fixup (k + 1)
                else if 10 ^ negate k * (r + mUp) <= s then k else fixup (k + 1)
            k = fixup k0
            generate ds rn sN mUpN mDnN =
              let (dn, rn2) = quotRem (rn * 10) sN
                  mUpN2 = mUpN * 10
                  mDnN2 = mDnN * 10
                  low = rn2 < mDnN2
                  high = rn2 + mUpN2 > sN
               in if low && not high
                    then dn : ds
                    else
                      if high && not low
                        then dn + 1 : ds
                        else
                          if low && high
                            then (if rn2 * 2 < sN then dn else dn + 1) : ds
                            else generate (dn : ds) rn2 sN mUpN2 mDnN2
            reversed =
              if k >= 0
                then generate [] r (s * 10 ^ k) mUp mDn
                else let bk = 10 ^ negate k in generate [] (r * bk) s (mUp * bk) (mDn * bk)
         in (map fromInteger (reverse reversed), k)

-- ---- As text ----

data FFFormat = FFExponent | FFFixed | FFGeneric

showFloat :: RealFloat a => a -> ShowS
showFloat x rest = formatRealFloat FFGeneric Nothing x ++ rest

showSignedFloat :: RealFloat a => Int -> a -> ShowS
showSignedFloat precedence x rest =
  if x < encodeFloat 0 0 || isNegativeZero x
    then
      if precedence > 6
        then '(' : '-' : showFloat (negate x) (')' : rest)
        else '-' : showFloat (negate x) rest
    else showFloat x rest

-- The number in the format, with that many digits after the point if a
-- number is given.
formatRealFloat :: RealFloat a => FFFormat -> Maybe Int -> a -> String
formatRealFloat format decimals x
  | isNaN x = "NaN"
  | isInfinite x = if x < encodeFloat 0 0 then "-Infinity" else "Infinity"
  | x < encodeFloat 0 0 || isNegativeZero x = '-' : formatDigits format decimals (floatToDigits (negate x))
  | otherwise = formatDigits format decimals (floatToDigits x)

formatDigits :: FFFormat -> Maybe Int -> ([Int], Int) -> String
formatDigits format decimals (is, e) = case format of
  FFGeneric -> formatDigits (if e < 0 || e > 7 then FFExponent else FFFixed) decimals (is, e)
  FFExponent -> case decimals of
    Nothing -> case ds of
      "0" -> "0.0e0"
      [d] -> d : ".0e" ++ show (e - 1)
      d : rest -> d : '.' : rest ++ "e" ++ show (e - 1)
      [] -> error "formatRealFloat: no digits"
    Just count ->
      if count <= 0
        then case is of
          [0] -> "0e0"
          _ -> case roundTo 1 is of
            (ei, rounded) -> case map intToDigit (if ei > 0 then init rounded else rounded) of
              n : _ -> n : 'e' : show (e - 1 + ei)
              [] -> error "formatRealFloat: no digits"
        else case is of
          [0] -> '0' : '.' : replicate count '0' ++ "e0"
          _ -> case roundTo (count + 1) is of
            (ei, rounded) -> case map intToDigit (if ei > 0 then init rounded else rounded) of
              d : rest -> d : '.' : rest ++ 'e' : show (e - 1 + ei)
              [] -> error "formatRealFloat: no digits"
  FFFixed -> case decimals of
    Nothing ->
      if e <= 0
        then "0." ++ replicate (negate e) '0' ++ ds
        else fixed e "" ds
    Just count ->
      let places = max count 0
       in if e >= 0
            then case roundTo (places + e) is of
              (ei, rounded) -> case splitAt (e + ei) (map intToDigit rounded) of
                (ls, rs) -> orZero ls ++ (if null rs then "" else '.' : rs)
            else case roundTo places (replicate (negate e) 0 ++ is) of
              (ei, rounded) -> case map intToDigit (if ei > 0 then rounded else 0 : rounded) of
                d : rest -> d : (if null rest then "" else '.' : rest)
                [] -> error "formatRealFloat: no digits"
  where
    ds = map intToDigit is
    orZero text = if null text then "0" else text
    -- Moves the point n places to the right through the digits.
    fixed n before after =
      if n == 0
        then orZero (reverse before) ++ '.' : orZero after
        else case after of
          [] -> fixed (n - 1) ('0' : before) []
          d : rest -> fixed (n - 1) (d : before) rest

-- The digits rounded to that many, and 1 if that added a digit in front.
-- A digit sequence that ends exactly in the middle is rounded to even.
roundTo :: Int -> [Int] -> (Int, [Int])
roundTo count is = case go count True is of
  (0, digits) -> (0, digits)
  (_, digits) -> (1, 1 : digits)
  where
    go n evenBefore digits = case digits of
      [] -> (0, replicate n 0)
      x : rest ->
        if n == 0
          then (if x == 5 && evenBefore && all (== 0) rest then 0 else if x >= 5 then 1 else 0, [])
          else case go (n - 1) (even x) rest of
            (carry, done) -> let sum' = carry + x in if sum' == 10 then (1, 0 : done) else (0, sum' : done)

-- ---- Reading ----

-- digits [. digits] [e [+ or -] digits], NaN or Infinity, with a minus
-- sign in front if there is one.
readFloat :: RealFloat a => String -> [(a, String)]
readFloat string = case dropWhile (== ' ') string of
  '-' : rest -> [(negate x, after) | (x, after) <- unsigned (dropWhile (== ' ') rest)]
  rest -> unsigned rest ++ [(x, after) | inside <- expect "(" string, (x, close) <- readFloat inside, after <- expect ")" close]
  where
    unsigned text = case text of
      'N' : 'a' : 'N' : rest -> [(encodeFloat 0 0 / encodeFloat 0 0, rest)]
      'I' : 'n' : 'f' : 'i' : 'n' : 'i' : 't' : 'y' : rest -> [(encodeFloat 1 0 / encodeFloat 0 0, rest)]
      _ -> case span isDigit text of
        ([], _) -> []
        (whole, rest) ->
          let (fraction, afterFraction) = case rest of
                '.' : more@(d : _) | isDigit d -> case span isDigit more of
                  (digits, after) -> ('.' : digits, after)
                _ -> ("", rest)
              (power, afterPower) = case afterFraction of
                e : more | e == 'e' || e == 'E' -> case more of
                  sign : digits@(d : _) | (sign == '-' || sign == '+') && isDigit d -> case span isDigit digits of
                    (ds, after) -> ('e' : sign : ds, after)
                  d : _ | isDigit d -> case span isDigit more of
                    (ds, after) -> ('e' : ds, after)
                  _ -> ("", afterFraction)
                _ -> ("", afterFraction)
           in [(fromRat (rationalLiteral (whole ++ fraction ++ power)), afterPower)]

-- ---- Sequences ----

-- From a number, in steps, without the error of adding up the steps.
floatEnumFromThen :: Fractional a => a -> a -> [a]
floatEnumFromThen from next = go 0
  where
    step = next - from
    go k = (from + fromInteger k * step) : go (k + 1)

-- The limit is passed by up to half a step, as the Haskell report says.
floatEnumFromThenTo :: (Ord a, Fractional a) => a -> a -> a -> [a]
floatEnumFromThenTo from next limit = takeWhile inRange (floatEnumFromThen from next)
  where
    half = (next - from) / fromInteger 2
    inRange x = if next >= from then x <= limit + half else x >= limit + half

-- ---- Bits ----

castDoubleToWord64 :: Double -> Word64
castDoubleToWord64 = unsafeCoerce

castWord64ToDouble :: Word64 -> Double
castWord64ToDouble = unsafeCoerce

castFloatToWord32 :: Float -> Word32
castFloatToWord32 = unsafeCoerce

castWord32ToFloat :: Word32 -> Float
castWord32ToFloat = unsafeCoerce

double2Float :: Double -> Float
double2Float = floatConvert

float2Double :: Float -> Double
float2Double = floatConvert

foreign import prim "unsafeCoerce" unsafeCoerce :: a -> b

foreign import prim "numAdd" numAdd :: a -> a -> a

foreign import prim "numSub" numSub :: a -> a -> a

foreign import prim "numMul" numMul :: a -> a -> a

foreign import prim "numEq" numEq :: a -> a -> Bool

foreign import prim "numLess" numLess :: a -> a -> Bool

foreign import prim "numConvert" numConvert :: a -> b

foreign import prim "floatDivide" floatDivide :: a -> a -> a

foreign import prim "floatMantissa" floatMantissa :: a -> Int

foreign import prim "floatExponent" floatExponent :: a -> Int

foreign import prim "floatEncode" floatEncode :: Int -> Int -> a

-- 0: an ordinary number, 1: NaN, 2: infinite, 3: negative zero, 4: too
-- small for a full mantissa.
foreign import prim "floatClass" floatClass :: a -> Int

-- One of C's functions, by number: see builtin_floatMath.
foreign import prim "floatMath" floatMath :: Int -> a -> a

foreign import prim "floatPower" floatPower :: a -> a -> a

foreign import prim "floatAtan2" floatAtan2 :: a -> a -> a

foreign import prim "floatConvert" floatConvert :: a -> b

-- ---- Double ----

instance Eq Double where
  (==) = numEq

instance Ord Double where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess
  (<=) a b = numLess a b || numEq a b
  (>) a b = numLess b a
  (>=) a b = numLess b a || numEq a b

instance Num Double where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  negate = floatMath 0
  abs = floatMath 1
  signum a = if a > 0 then 1 else if a < 0 then negate 1 else a
  fromInteger = numConvert

instance Real Double where
  toRational = floatToRational

instance Fractional Double where
  (/) = floatDivide
  fromRational = fromRat

instance RealFrac Double where
  properFraction = floatProperFraction

instance Floating Double where
  pi = 3.141592653589793238
  sqrt = floatMath 2
  exp = floatMath 3
  log = floatMath 4
  sin = floatMath 5
  cos = floatMath 6
  tan = floatMath 7
  asin = floatMath 8
  acos = floatMath 9
  atan = floatMath 10
  sinh = floatMath 11
  cosh = floatMath 12
  tanh = floatMath 13
  (**) = floatPower

instance RealFloat Double where
  floatDigits _ = 53
  floatRange _ = (-1021, 1024)
  decodeFloat a = (toInteger (floatMantissa a), floatExponent a)
  encodeFloat = encodeBig floatEncode
  isNaN a = floatClass a == 1
  isInfinite a = floatClass a == 2
  isNegativeZero a = floatClass a == 3
  isDenormalized a = floatClass a == 4
  atan2 = floatAtan2

instance Show Double where
  showsPrec = showSignedFloat

instance Read Double where
  readsPrec _ = readFloat

instance Enum Double where
  succ a = a + 1
  pred a = a - 1
  toEnum n = fromInteger (toInteger n)
  fromEnum = truncate
  enumFrom a = floatEnumFromThen a (a + 1)
  enumFromThen = floatEnumFromThen
  enumFromTo a b = floatEnumFromThenTo a (a + 1) b
  enumFromThenTo = floatEnumFromThenTo

-- ---- Float ----

instance Eq Float where
  (==) = numEq

instance Ord Float where
  compare a b = if numLess a b then LT else if numEq a b then EQ else GT
  (<) = numLess
  (<=) a b = numLess a b || numEq a b
  (>) a b = numLess b a
  (>=) a b = numLess b a || numEq a b

instance Num Float where
  (+) = numAdd
  (-) = numSub
  (*) = numMul
  negate = floatMath 0
  abs = floatMath 1
  signum a = if a > 0 then 1 else if a < 0 then negate 1 else a
  fromInteger = numConvert

instance Real Float where
  toRational = floatToRational

instance Fractional Float where
  (/) = floatDivide
  fromRational = fromRat

instance RealFrac Float where
  properFraction = floatProperFraction

instance Floating Float where
  pi = 3.141592653589793238
  sqrt = floatMath 2
  exp = floatMath 3
  log = floatMath 4
  sin = floatMath 5
  cos = floatMath 6
  tan = floatMath 7
  asin = floatMath 8
  acos = floatMath 9
  atan = floatMath 10
  sinh = floatMath 11
  cosh = floatMath 12
  tanh = floatMath 13
  (**) = floatPower

instance RealFloat Float where
  floatDigits _ = 24
  floatRange _ = (-125, 128)
  decodeFloat a = (toInteger (floatMantissa a), floatExponent a)
  encodeFloat = encodeBig floatEncode
  isNaN a = floatClass a == 1
  isInfinite a = floatClass a == 2
  isNegativeZero a = floatClass a == 3
  isDenormalized a = floatClass a == 4
  atan2 = floatAtan2

instance Show Float where
  showsPrec = showSignedFloat

instance Read Float where
  readsPrec _ = readFloat

instance Enum Float where
  succ a = a + 1
  pred a = a - 1
  toEnum n = fromInteger (toInteger n)
  fromEnum = truncate
  enumFrom a = floatEnumFromThen a (a + 1)
  enumFromThen = floatEnumFromThen
  enumFromTo a b = floatEnumFromThenTo a (a + 1) b
  enumFromThenTo = floatEnumFromThenTo
