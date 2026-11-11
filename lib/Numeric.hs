module Numeric where

-- Numbers in other bases, and floating point numbers in other formats.

import Data.Char (digitToInt, intToDigit, isDigit, isHexDigit, isOctDigit)
import GHC.Float (FFFormat (..), floatToDigits, formatRealFloat, readFloat, showFloat)
import Text.Read (ReadS)

-- The digits of a number that is not negative, in the base, in front of
-- the string.
showIntAtBase :: Integral a => a -> (Int -> Char) -> a -> ShowS
showIntAtBase base toDigit n rest
  | n < 0 = error "Numeric.showIntAtBase: applied to negative number"
  | otherwise = go (quotRem n base) rest
  where
    go (q, r) text =
      let text' = toDigit (fromIntegral r) : text
       in if q == 0 then text' else go (quotRem q base) text'

showHex :: Integral a => a -> ShowS
showHex n rest = if n < 0 then error "Numeric.showHex: can't show negative numbers" else showIntAtBase 16 intToDigit n rest

showOct :: Integral a => a -> ShowS
showOct n rest = if n < 0 then error "Numeric.showOct: can't show negative numbers" else showIntAtBase 8 intToDigit n rest

showBin :: Integral a => a -> ShowS
showBin n rest = if n < 0 then error "Numeric.showBin: can't show negative numbers" else showIntAtBase 2 intToDigit n rest

showInt :: Integral a => a -> ShowS
showInt n rest = if n < 0 then error "Numeric.showInt: can't show negative numbers" else showIntAtBase 10 intToDigit n rest

-- The number that the digits at the start of the string are, in the base.
-- Nothing is read if the string does not start with a digit.
readInt :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadS a
readInt base isDigitOfBase valueOf string = case span isDigitOfBase string of
  ([], _) -> []
  (digits, rest) -> [(foldl (\n d -> n * base + fromIntegral (valueOf d)) 0 digits, rest)]

readHex :: (Eq a, Num a) => ReadS a
readHex = readInt 16 isHexDigit digitToInt

readOct :: (Eq a, Num a) => ReadS a
readOct = readInt 8 isOctDigit digitToInt

readDec :: (Eq a, Num a) => ReadS a
readDec = readInt 10 isDigit digitToInt

readBin :: (Eq a, Num a) => ReadS a
readBin = readInt 2 (\c -> c == '0' || c == '1') digitToInt

-- With that many digits after the point, if a number is given:
-- 1234.5 as 1234.5, as 1.2345e3, and as whichever of the two `show` takes.
showFFloat :: RealFloat a => Maybe Int -> a -> ShowS
showFFloat decimals x rest = formatRealFloat FFFixed decimals x ++ rest

showEFloat :: RealFloat a => Maybe Int -> a -> ShowS
showEFloat decimals x rest = formatRealFloat FFExponent decimals x ++ rest

showGFloat :: RealFloat a => Maybe Int -> a -> ShowS
showGFloat decimals x rest = formatRealFloat FFGeneric decimals x ++ rest
