module Main where

import Data.Bits (complement, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.Int (Int32, Int64, Int8)
import Data.Word (Word16, Word64, Word8)

factorial :: Integer -> Integer
factorial n = product [1 .. n]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- Numbers around every size where the form of an Integer changes.
edges :: [Integer]
edges =
  concat
    [ [p - 1, p, p + 1, negate p - 1, negate p, negate p + 1]
      | k <- [31, 32, 62, 63, 64, 65, 95, 96, 127, 128 :: Int],
        let p = 2 ^ k
    ]
    ++ [0, 1, -1, 7, -7, 1000000007, 123456789012345678901234567890, -987654321098765432109876543210]

main :: IO ()
main = do
  print (factorial 25, factorial 30)
  print (fibs !! 150)
  print (2 ^ (64 :: Int) :: Integer, 2 ^ (200 :: Int) :: Integer, (-3) ^ (41 :: Int) :: Integer)
  print (18446744073709551616, 0xFFFFFFFFFFFFFFFFFFFF, -340282366920938463463374607431768211456 :: Integer)
  print (maxBound :: Int, toInteger (maxBound :: Int) + 1, toInteger (minBound :: Int) - 1, toInteger (maxBound :: Word64) * 2)
  print (sum edges, product (filter (/= 0) (take 12 edges)))
  -- Every pair of the edge numbers through the arithmetic.
  let pairs = [(a, b) | a <- edges, b <- edges]
  print (sum [a + b | (a, b) <- pairs], sum [a - b | (a, b) <- pairs] == 0, sum [a * b | (a, b) <- pairs])
  print (length [() | (a, b) <- pairs, b /= 0, (a `quot` b) * b + (a `rem` b) /= a])
  print (length [() | (a, b) <- pairs, b /= 0, (a `div` b) * b + (a `mod` b) /= a])
  print (sum [a `quot` b | (a, b) <- pairs, b /= 0], sum [a `rem` b | (a, b) <- pairs, b /= 0])
  print (sum [a `div` b | (a, b) <- pairs, b /= 0], sum [a `mod` b | (a, b) <- pairs, b /= 0])
  print (map (\(a, b) -> (a `divMod` b, a `quotRem` b)) [(2 ^ (70 :: Int), 7), (negate (2 ^ (70 :: Int)), 7), (2 ^ (70 :: Int), -7), (negate (2 ^ (70 :: Int)), -7), (5, 2 ^ (70 :: Int)), (-5, 2 ^ (70 :: Int))])
  print (length [() | (a, b) <- pairs, compare a b /= compare (a - b) 0], length [() | (a, b) <- pairs, (a == b) /= (a - b == 0)])
  print (maximum edges, minimum edges, length (filter (< 0) edges), signum (minimum edges), abs (minimum edges))
  print (gcd (factorial 20) (2 ^ (70 :: Int)), lcm (2 ^ (40 :: Int)) (3 ^ (30 :: Int)) :: Integer, gcd (-12) 18 :: Integer)
  -- Bits.
  print (map (\n -> shiftL 1 n :: Integer) [0, 31, 32, 63, 64, 100])
  print (map (\n -> shiftR (2 ^ (100 :: Int) + 12345) n :: Integer) [0, 1, 36, 64, 99, 100, 101, 500])
  print (map (\n -> shiftR (negate (2 ^ (100 :: Int)) - 12345) n :: Integer) [0, 1, 36, 64, 99, 100, 101, 500])
  print (shiftL (-5) 70 :: Integer, shiftR (-1) 3 :: Integer, shiftR (-9) 1 :: Integer)
  let big = 2 ^ (90 :: Int) + 0xF0F0 :: Integer
  print (big .&. 0xFF00, big .|. 0x0F0F, big `xor` big, big `xor` 1, big .&. (2 ^ (90 :: Int)))
  print ((-big) .&. 0xFFFF, (-big) .|. 0xFFFF, (-big) `xor` big, (-1) .&. big == big, complement big, complement (-big))
  print ((-big) .&. (-(2 ^ (70 :: Int))), (-big) .|. (-(2 ^ (70 :: Int))), (-big) `xor` (-(2 ^ (70 :: Int))))
  print (map (testBit big) [0, 4, 15, 16, 89, 90, 91], map (testBit (-big)) [0, 4, 15, 16, 90, 91, 200])
  -- To and from the fixed sizes.
  print (fromInteger (2 ^ (70 :: Int) + 300) :: Int, fromInteger (2 ^ (70 :: Int) + 300) :: Word8, fromInteger (negate (2 ^ (70 :: Int)) - 1) :: Int8)
  print (fromInteger (2 ^ (64 :: Int) - 1) :: Word64, fromInteger (2 ^ (64 :: Int) - 1) :: Int64, fromInteger (2 ^ (63 :: Int)) :: Int, fromInteger (2 ^ (31 :: Int)) :: Int32)
  print (toInteger (maxBound :: Word64), toInteger (maxBound :: Word64) + 1, fromIntegral (maxBound :: Word64) * (3 :: Integer), fromIntegral (2 ^ (16 :: Int) + 5 :: Integer) :: Word16)
  -- Text.
  print (map show [10 ^ (18 :: Int), 10 ^ (19 :: Int), 10 ^ (27 :: Int) + 1, negate (10 ^ (36 :: Int)) :: Integer])
  print (read "123456789012345678901234567890" :: Integer, read "-18446744073709551616" :: Integer, read "0xFFFFFFFFFFFFFFFFFF" :: Integer)
  print (length (show (factorial 200)), sum (map (\c -> fromEnum c - 48) (show (factorial 200))), factorial 200 `mod` 1000000007)
  print (showsPrec 7 (negate (2 ^ (80 :: Int)) :: Integer) "", [2 ^ (64 :: Int) .. 2 ^ (64 :: Int) + 3 :: Integer])
