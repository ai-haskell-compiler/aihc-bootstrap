module Main where

-- Needs the packages vector and primitive, which GHC does not ship. The
-- expected output was checked by hand, not generated with runghc as for
-- most tests.

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import qualified Data.ByteString as B
import Data.ByteString.Short (toShort)
import Data.ByteString.Short.Internal (ShortByteString (SBS), fromShort)
import Data.Primitive.ByteArray (ByteArray (..), copyByteArray, moveByteArray, newPinnedByteArray, readByteArray, unsafeFreezeByteArray, writeByteArray)
import Data.Primitive.MutVar (modifyMutVar', newMutVar, readMutVar, writeMutVar)
import Data.Primitive.PrimArray (newPrimArray, readPrimArray, setPrimArray, writePrimArray)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Data.Word (Word8)

-- A vector that grows, as the points-to analysis keeps its sets.
squares :: Int -> [Int]
squares count = runST (do
  start <- MU.new 2
  let go vector i
        | i >= count = pure vector
        | i < MU.length vector = MU.write vector i (i * i) >> go vector (i + 1)
        | otherwise = MU.grow vector (MU.length vector) >>= \larger -> go larger i
  vector <- go start 0
  mapM (MU.read vector) [0 .. count - 1])

boxed :: [String]
boxed = runST (do
  vector <- MV.replicate 4 "-"
  MV.write vector 1 "one"
  MV.modify vector (++ "!") 1
  MV.set (MV.slice 2 2 vector) "tail"
  MV.write (MV.take 1 vector) 0 "head"
  frozen <- V.freeze vector
  MV.write vector 0 "changed after the copy"
  pure (V.toList frozen))

zeros :: (Int, Bool, Word8, Int)
zeros = runST (do
  ints <- MU.new 3
  flags <- MU.new 3
  bytes <- MU.new 3
  MU.unsafeWrite ints 2 7
  a <- MU.unsafeRead ints 0
  b <- MU.unsafeRead flags 1
  c <- MU.unsafeRead bytes 2
  total <- MU.foldr (+) 0 ints
  pure (a, b, c, total))

counters :: [Int]
counters = runST (do
  array <- newPrimArray 4
  setPrimArray array 0 4 0
  forM_ [0 .. 9] (\i -> readPrimArray array (i `mod` 4) >>= \n -> writePrimArray array (i `mod` 4) (n + i))
  mapM (readPrimArray array) [0 .. 3])

cell :: (Int, [Int])
cell = runST (do
  sum' <- newMutVar 0
  seen <- newMutVar []
  forM_ [1 .. 5] (\n -> modifyMutVar' sum' (+ n) >> readMutVar seen >>= \list -> writeMutVar seen (n : list))
  (,) <$> readMutVar sum' <*> readMutVar seen)

-- Bytes written in place and then read as a ByteString, as the object
-- writer does.
chunk :: [Word8]
chunk = runST (do
  bytes <- newPinnedByteArray 8
  forM_ [0 .. 3] (\i -> writeByteArray bytes i (fromIntegral (i + 1) :: Word8))
  let source = case toShort (B.pack [9, 8, 7]) of SBS array -> ByteArray array
  copyByteArray bytes 4 source 1 2
  moveByteArray bytes 1 bytes 0 3
  first <- readByteArray bytes 0
  ByteArray frozen <- unsafeFreezeByteArray bytes
  pure (first : B.unpack (B.take 7 (fromShort (SBS frozen)))))

main :: IO ()
main = do
  print (squares 10)
  print boxed
  print zeros
  print counters
  print cell
  print chunk
  let vector = U.fromList "vector"
  print (U.length vector, U.unsafeIndex vector 2, U.length (U.empty :: U.Vector Int), V.toList (V.fromList [1, 2, 3 :: Int]))
