module Main where

import Data.Ratio (denominator, numerator, (%))
import Data.Word (Word32, Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, castWord32ToFloat, castWord64ToDouble, double2Float, float2Double)
import Numeric (showEFloat, showFFloat, showGFloat, showHex)

-- The bits of a number: what a compiler writes into an object file.
bits :: Double -> String
bits x = showHex (castDoubleToWord64 x) ""

bitsF :: Float -> String
bitsF x = showHex (castFloatToWord32 x) ""

doubles :: [Double]
doubles =
  [ 0, 1, -1, 0.1, 0.2, 0.3, 0.1 + 0.2, 1 / 3, 2 / 3, 1.5, 2.5, 3.5, 100, 1e7, 12345678.9, 1e21, 1e22, 1e23, 1.0e-2, 0.1e-1, 5.0e-324, 2.2250738585072014e-308,
    1.7976931348623157e308, 4.9e-324, 2.225073858507201e-308, 123456789.123456789, 9007199254740993, 9007199254740992, 0.000001, 0.0000001, 1e-7, 3.141592653589793,
    2.718281828459045, 6.02214076e23, 1.602176634e-19, 299792458, 0.5, 0.25, 0.125, 1e100, 1e-100, 123.456, 9.999999999999999e22, 1.2345678901234567e-5
  ]

main :: IO ()
main = do
  -- Fractions.
  print (3 % 4 + 1 % 4 :: Rational, 6 % (-8) :: Rational, numerator (10 % 4 :: Rational), denominator (10 % 4 :: Rational))
  print ((1 % 3) * (3 % 5) - (1 % 5) :: Rational, recip (-3 % 7 :: Rational), (2 % 3) / (4 % 9) :: Rational, (2 % 3) ^ (5 :: Int) :: Rational, (2 % 3) ^^ (-2 :: Int) :: Rational)
  print (compare (1 % 3) (2 % 6 :: Rational), (1 % 3 :: Rational) < 0.34, maximum [1 % 2, 2 % 3, 3 % 5 :: Rational], signum (-1 % 2 :: Rational), abs (-1 % 2 :: Rational))
  print (map (\r -> (truncate r, round r, floor r, ceiling r) :: (Integer, Integer, Integer, Integer)) [7 % 2, -7 % 2, 5 % 2, -5 % 2, 1 % 3, -1 % 3, 2, 0 :: Rational])
  print (toRational (0.75 :: Double), toRational (0.1 :: Double), fromRational (7 % 8) :: Double, toRational (5 :: Int), 0.125 :: Rational, 1e3 :: Rational, 1.5e-3 :: Rational)
  print (showsPrec 8 (1 % 2 :: Rational) "", [1 % 2, -3 % 4 :: Rational], Just (5 % 1 :: Rational))
  -- Showing: the shortest digits that mean the same number.
  mapM_ (\x -> putStrLn (show x ++ " " ++ bits x)) doubles
  print (map negate [0, 1.5, 1e-3 :: Double], [1 / 0, -1 / 0 :: Double], isNaN (0 / 0 :: Double), show (0 / 0 :: Double))
  print (Just (-1.5 :: Double), [0.1, -0.2 :: Double], (1.0e-2 :: Double, 5.0e7 :: Double), showsPrec 7 (-2.0 :: Double) "")
  print (map (\x -> (double2Float x, bitsF (double2Float x))) [0.1, 1 / 3, 16777217, 1e39, 1e-46, 3.4028235e38, 1.17549435e-38, 1.0e-45])
  print (0.1 :: Float, 0.1 + 0.2 :: Float, float2Double 0.1, 16777216 + 1 :: Float, 3.4028235e38 :: Float, 1.0e-45 :: Float, 1.17549435e-38 :: Float)
  -- Literals and reading are rounded once, to the nearest number.
  print (map bits [0.1, 1e23, 8.98846567431158e307, 2.2250738585072011e-308, 4.9406564584124654e-324, 2.4703282292062328e-324, 2.4703282292062327e-324, 1.7976931348623159e308, 9007199254740993.0, 0.30000000000000004])
  print (map bits [fromRational (1 % 3), fromRational (2 ^ (1024 :: Int) % 1), fromRational (1 % (2 ^ (1075 :: Int))), fromRational (3 % (2 ^ (1075 :: Int))), fromIntegral (2 ^ (53 :: Int) + 1 :: Integer), fromIntegral (2 ^ (80 :: Int) + 2 ^ (27 :: Int) + 1 :: Integer), fromIntegral (maxBound :: Int), fromIntegral (maxBound :: Word64)])
  print (map bitsF [0.1, 16777217, fromIntegral (2 ^ (24 :: Int) + 1 :: Int), fromIntegral (2 ^ (60 :: Int) + 2 ^ (36 :: Int) + 1 :: Integer), fromRational (1 % 3), 1e-45, 7e-46])
  print (read "1.5" :: Double, read " -2.5e-3 " :: Double, read "1e3" :: Double, read "17" :: Double, read "NaN" :: Double, read "Infinity" :: Double, read "0.1" :: Float, (reads :: ReadS Double) "1.x", (reads :: ReadS Double) ".5")
  print (castWord64ToDouble 0x400921FB54442D18, castWord32ToFloat 0x40490FDB, castDoubleToWord64 (-0.0), castFloatToWord32 (-0.0), (castWord32ToFloat (castFloatToWord32 1.5 + 1) :: Float))
  -- Arithmetic.
  print (0.1 + 0.2 == (0.3 :: Double), 1 / 3 * 3 == (1 :: Double), 2 / 3, 7 / 2, 2 ** 10, sqrt 2 :: Double, 10 ^ (3 :: Int) / 4, 2 ^^ (-3 :: Int), fromIntegral (7 :: Int) / 2)
  print (map (\x -> (truncate x, round x, floor x, ceiling x) :: (Int, Int, Int, Int)) [2.5, 3.5, -2.5, -3.5, 2.4, -2.6, 0.5, 1.5, 1e10, -0.0 :: Double])
  print (properFraction (3.75 :: Double) :: (Int, Double), properFraction (-3.75 :: Double) :: (Integer, Double), truncate (1e30 :: Double) :: Integer, round (2 ** 70 :: Double) :: Integer)
  print (decodeFloat (1 :: Double), decodeFloat (0.1 :: Double), decodeFloat (5.0e-324 :: Double), decodeFloat (0 :: Double), decodeFloat (1.5 :: Float), encodeFloat 3 4 :: Double, encodeFloat 1 (-1074) :: Double)
  print (floatDigits (1 :: Double), floatRange (1 :: Double), floatDigits (1 :: Float), floatRange (1 :: Float), exponent (8 :: Double), significand (8 :: Double), scaleFloat 3 (1.5 :: Double))
  print (isNaN (0 / 0 :: Double), isInfinite (1 / 0 :: Double), isNegativeZero (-0.0 :: Double), isNegativeZero (0 :: Double), isDenormalized (5.0e-324 :: Double), isDenormalized (1 :: Double), 0 / 0 == (0 / 0 :: Double), (0 / 0 :: Double) < 1, (0 / 0 :: Double) >= 1)
  print (compare 1 (2 :: Double), max 1.5 (2.5 :: Double), min (-0.0) (0 :: Double), abs (-2.5 :: Double), signum (-2.5 :: Double), signum (0 :: Double), negate 0 :: Double, realToFrac (1.5 :: Float) :: Double, realToFrac (0.1 :: Double) :: Float)
  print ([1.0, 1.5 .. 3.0 :: Double], [1 .. 4.5 :: Double], [10, 8 .. 1 :: Double], take 3 [0.1 :: Double ..], [1.0 .. 0 :: Double], succ 1.5 :: Double, fromEnum (3.7 :: Double), toEnum 5 :: Double)
  print (pi :: Double, exp 1 :: Double, log 10 :: Double, sin 0 :: Double, cos 0 :: Double, atan2 1 1 * 4 :: Double, logBase 2 1024 :: Double, pi :: Float)
  -- Defaulting: a fraction that nothing else gives a type is a Double.
  print (1.5, 2 / 3, 10 / 4 + 1, 1e3, truncate 2.7 + 1, sqrt 16, 2 ** 0.5)
  let seconds = fromIntegral (1234567890 :: Word64) / 1e9 :: Double
  -- Formats.
  putStrLn (showFFloat (Just 1) seconds " s" ++ " " ++ showFFloat (Just 3) seconds "" ++ " " ++ showFFloat Nothing seconds "" ++ " " ++ showFFloat (Just 0) seconds "")
  print (map (\d -> showFFloat (Just d) (2.675 :: Double) "") [0, 1, 2, 3], map (\x -> showFFloat (Just 1) (x :: Double) "") [0.05, 0.15, 0.25, 0.35, 9.95, 99.95, -0.04, 0, 1e21, 1.0e-2, 123456.789])
  print (map (\x -> showEFloat (Just 2) (x :: Double) "") [0, 1, 1234.5, 0.000123, 9.995, -5.5e10], showEFloat Nothing (1234.5 :: Double) "", showEFloat (Just 0) (1234.5 :: Double) "")
  print (map (\x -> showGFloat (Just 2) (x :: Double) "") [0.05, 1234.5, 1e8, 1e-3], showGFloat Nothing (1e7 :: Double) "", showGFloat Nothing (0.1 :: Double) "")
