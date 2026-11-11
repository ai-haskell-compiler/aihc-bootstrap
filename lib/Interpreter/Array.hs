module Interpreter.Array where

-- The interpreter's own array: a block of elements that can be changed in
-- place. Data.Array, the vectors and the primitive arrays are all this,
-- each with its own interface. An immutable array is one that is not
-- changed any more.

import System.IO.Unsafe (unsafePerformIO)

data MutableArray a

foreign import prim "arrayNew" new :: Int -> a -> IO (MutableArray a)

foreign import prim "arrayRead" read :: MutableArray a -> Int -> IO a

foreign import prim "arrayWrite" write :: MutableArray a -> Int -> a -> IO ()

foreign import prim "arrayLength" length :: MutableArray a -> Int

-- An element of an array that is not changed any more.
foreign import prim "arrayIndex" index :: MutableArray a -> Int -> a

-- copy source from target to count
foreign import prim "arrayCopy" copy :: MutableArray a -> Int -> MutableArray a -> Int -> Int -> IO ()

-- What an element is before anything is written to it.
uninitialised :: a
uninitialised = error "an array element that was never written"

-- A new array of that length with the elements of the list at its start.
fromListN :: Int -> [a] -> MutableArray a
fromListN count list = unsafePerformIO (new count uninitialised >>= \array -> fill array 0 list)
  where
    fill array i elements = case elements of
      a : rest | i < count -> write array i a >> fill array (i + 1) rest
      _ -> pure array

toList :: MutableArray a -> [a]
toList array = [index array i | i <- [0 .. length array - 1]]

-- A new array with that part of the array.
clone :: MutableArray a -> Int -> Int -> IO (MutableArray a)
clone array from count = new count uninitialised >>= \result ->
  copy array from result 0 count >> pure result
