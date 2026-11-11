module Data.Ratio (module Data.Ratio, Ratio, Rational) where

-- Fractions. The type is in the Prelude: numerator :% denominator, always
-- in lowest terms and with a positive denominator, so that equal fractions
-- are the same value.

infixl 7 %

(%) :: Integral a => a -> a -> Ratio a
(%) x y = reduce (x * signum y) (abs y)

reduce :: Integral a => a -> a -> Ratio a
reduce x y =
  if y == 0
    then error "Ratio has zero denominator"
    else let d = gcd x y in quot x d :% quot y d

numerator :: Ratio a -> a
numerator (x :% _) = x

denominator :: Ratio a -> a
denominator (_ :% y) = y

instance Eq a => Eq (Ratio a) where
  (==) (x :% y) (x' :% y') = x == x' && y == y'

instance Integral a => Ord (Ratio a) where
  compare (x :% y) (x' :% y') = compare (x * y') (x' * y)

instance Integral a => Num (Ratio a) where
  (+) (x :% y) (x' :% y') = reduce (x * y' + x' * y) (y * y')
  (-) (x :% y) (x' :% y') = reduce (x * y' - x' * y) (y * y')
  (*) (x :% y) (x' :% y') = reduce (x * x') (y * y')
  negate (x :% y) = negate x :% y
  abs (x :% y) = abs x :% y
  signum (x :% _) = signum x :% 1
  fromInteger n = fromInteger n :% 1

instance Integral a => Fractional (Ratio a) where
  (/) (x :% y) (x' :% y') = (x * y') % (y * x')
  recip (x :% y)
    | x == 0 = error "Ratio has zero denominator"
    | x < 0 = negate y :% negate x
    | otherwise = y :% x
  fromRational (x :% y) = fromInteger x % fromInteger y

instance Integral a => Real (Ratio a) where
  toRational (x :% y) = toInteger x :% toInteger y

instance Integral a => RealFrac (Ratio a) where
  properFraction (x :% y) = case quotRem x y of
    (q, r) -> (fromInteger (toInteger q), r :% y)

instance Show a => Show (Ratio a) where
  showsPrec precedence (x :% y) rest =
    if precedence > 7
      then '(' : inner (')' : rest)
      else inner rest
    where
      inner end = showsPrec 8 x (" % " ++ showsPrec 8 y end)

-- The number a decimal literal stands for:
-- digits [. digits] [e [+ or -] digits], with underscores anywhere.
rationalLiteral :: String -> Rational
rationalLiteral text = case break isExponent (filter (/= '_') text) of
  (mantissa, power) -> case break (== '.') mantissa of
    (whole, fraction) ->
      let decimals = drop 1 fraction
          n = digitsValue (whole ++ decimals)
          e = exponentValue (drop 1 power) - length decimals
       in if e >= 0 then (n * 10 ^ e) :% 1 else n % (10 ^ negate e)
  where
    isExponent c = c == 'e' || c == 'E'
    digitsValue digits = foldl (\total d -> total * 10 + toInteger (fromEnum d - fromEnum '0')) 0 digits
    exponentValue digits = case digits of
      '-' : rest -> negate (fromInteger (digitsValue rest))
      '+' : rest -> fromInteger (digitsValue rest)
      _ -> fromInteger (digitsValue digits)
