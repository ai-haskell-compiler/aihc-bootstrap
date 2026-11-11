module Main where

import Data.Int (Int16, Int32, Int8)
import Data.Word (Word16, Word32, Word64, Word8)

-- Types fix how each result wraps around.
bytes :: [Word8]
bytes = [250 + 10, 3 - 5, 16 * 17, negate 1, fromIntegral (1000 :: Int)]

small :: [Int8]
small = [127 + 1, negate 128 - 1, 100 * 3, fromIntegral (200 :: Word8)]

wide :: [Word64]
wide = [18446744073709551615, 18446744073709551615 + 2, 15958160436627155571, 0 - 1]

signed :: [Int]
signed = [maxBound, minBound, maxBound + 1, minBound - 1, negate 7]

-- Rounding of the two kinds of division, for every combination of signs.
divisions :: [(Int, Int, Int, Int)]
divisions = [(quot a b, rem a b, div a b, mod a b) | a <- [7, -7], b <- [2, -2]]

unsignedDivision :: (Word64, Word64, Bool)
unsignedDivision = (quot 18446744073709551615 3, rem 18446744073709551615 10, 18446744073709551615 > (1 :: Word64))

main :: IO ()
main = do
  print bytes
  print small
  print wide
  print signed
  print divisions
  print unsignedDivision
  print (map fromIntegral bytes :: [Int])
  print (fromIntegral (maxBound :: Word64) :: Int, fromIntegral (-1 :: Int) :: Word32)
  print (fromIntegral (40000 :: Word16) :: Int16, fromIntegral (3000000000 :: Word32) :: Int32)
  -- Precedence of negation, and negative numbers in show.
  print (-2 * 3 + 1, - 2 ^ 2, 10 - (-3), [-1, 1 - 2])
  print (Just (-5 :: Int), compare 3 (4 :: Int), max 3 (-4 :: Integer))
  -- Ranges.
  print [1 .. 5 :: Int]
  print [10, 8 .. 1 :: Int]
  print [1, 3 .. 10 :: Integer]
  print [5 .. 1 :: Int]
  print (take 4 [3 ..] :: [Integer])
  print (take 3 [0, 5 ..] :: [Int])
  print ['a' .. 'f']
  print ([250 ..] :: [Word8])
  print ([254, 255 ..] :: [Word8])
  print [x * x | x <- [1 .. 10 :: Int], odd x]
  print [(x, y) | x <- [1 .. 3 :: Int], y <- "ab", let z = x + 1, even z]
  -- Other functions over numbers.
  print (sum [1 .. 100 :: Int], product [1 .. 10 :: Integer], length "hello")
  print (gcd 12 (18 :: Int), lcm 4 (6 :: Int), 2 ^ (10 :: Int) :: Integer, divMod (-7) (2 :: Int))
  print (minimum [3, 1, 2 :: Int], maximum "hello", abs (-3 :: Int), signum (-3 :: Integer))
  print (succ 'a', pred (10 :: Int), fromEnum 'A', toEnum 98 :: Char)
  print (words "  several   words here ", lines "one\ntwo\n\nfour", unwords ["a", "b"])
  print (zip [1 :: Int ..] "abc", lookup 2 (zip [1 :: Int ..] "abc"), reverse [1, 2, 3 :: Int])
  print (show 'x', show "quote\" tab\t nul\0 del\DEL uni\955", "\SOH", "\1234\&5")
