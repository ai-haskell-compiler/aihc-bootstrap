module Main where

import Control.Monad (forM_, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (getNumElements)
import Data.Array.ST (STUArray, getBounds, newArray, readArray, writeArray)
import Data.Array.Unboxed (Array, UArray, accumArray, array, assocs, bounds, elems, indices, listArray, (!))
import Data.Array.Unsafe (unsafeFreeze)
import Data.Word (Word64)

-- The primes below a limit, with an array of flags changed in place.
sieve :: Int -> [Int]
sieve limit = [n | (n, True) <- assocs flags]
  where
    flags :: UArray Int Bool
    flags = runST (do
      marks <- newArray (2, limit) True :: ST s (STUArray s Int Bool)
      forM_ [2 .. limit] (\n -> do
        prime <- readArray marks n
        when (prime && n * n <= limit) (forM_ [n * n, n * n + n .. limit] (\m -> writeArray marks m False)))
      unsafeFreeze marks)

-- Running sums, as the flat LIR does: counts, then the sums in place.
prefixSums :: [Int] -> UArray Int Int
prefixSums values = runST (do
  let size = length values
  sums <- newArray (0, size) 0 :: ST s (STUArray s Int Int)
  forM_ (zip [0 ..] values) (\(i, v) -> writeArray sums (i + 1) v)
  count <- getNumElements sums
  forM_ [1 .. count - 1] (\i -> do
    before <- readArray sums (i - 1)
    here <- readArray sums i
    writeArray sums i (before + here))
  unsafeFreeze sums)

bits :: Int -> Word64
bits n = runST (do
  words' <- newArray (0, 3) 0 :: ST s (STUArray s Int Word64)
  forM_ [0 .. n] (\i -> readArray words' (i `mod` 4) >>= \w -> writeArray words' (i `mod` 4) (w * 3 + fromIntegral i))
  limits <- getBounds words'
  total <- mapM (readArray words') [fst limits .. snd limits]
  pure (sum total))

main :: IO ()
main = do
  print (sieve 60)
  print (elems (prefixSums [3, 1, 4, 1, 5, 9, 2, 6]))
  print (bounds (prefixSums [1, 2, 3]), prefixSums [1, 2, 3] ! 3)
  print (bits 100)
  let names = listArray (10, 13) ["ten", "eleven", "twelve", "thirteen"] :: Array Int String
  print (names ! 10, names ! 13, bounds names, indices names)
  print (assocs (fmap length names))
  let byChar = listArray ('a', 'e') [1 ..] :: UArray Char Int
  print (byChar ! 'c', assocs byChar)
  print (elems (array (1, 3) [(3, 'c'), (1, 'a'), (2, 'b')] :: Array Int Char))
  print (elems (accumArray (flip (:)) [] (0, 3) [(1, 'a'), (3, 'b'), (1, 'c'), (0, 'd')] :: Array Int String))
  print (elems (accumArray (+) 0 (0, 2) [(0, 1), (2, 5), (0, 10)] :: UArray Int Int))
  print (elems (listArray (0, -1) [] :: Array Int Int), bounds (listArray (5, 4) "" :: UArray Int Char))
  -- An element nobody reads may be anything.
  print ((listArray (0, 2) [1, undefined, 3] :: Array Int Int) ! 2)
  print (length (elems (listArray (0, 99999) [0 ..] :: UArray Int Int)), (listArray (0, 99999) [0 ..] :: UArray Int Int) ! 77777)
