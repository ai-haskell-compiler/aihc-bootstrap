module Data.Primitive.PrimArray where

-- An array of numbers that is changed in place, in ST.

import Control.Monad.ST (ST (..))
import Interpreter.Array (MutableArray)
import qualified Interpreter.Array as A

newtype MutablePrimArray s a = MutablePrimArray (MutableArray a)

-- The elements are not to be read before they are written.
newPrimArray :: Int -> ST s (MutablePrimArray s a)
newPrimArray count = ST (fmap MutablePrimArray (A.new count A.uninitialised))

readPrimArray :: MutablePrimArray s a -> Int -> ST s a
readPrimArray (MutablePrimArray elements) i = ST (A.read elements i)

writePrimArray :: MutablePrimArray s a -> Int -> a -> ST s ()
writePrimArray (MutablePrimArray elements) i a = ST (A.write elements i a)

-- setPrimArray array from count value
setPrimArray :: MutablePrimArray s a -> Int -> Int -> a -> ST s ()
setPrimArray array from count a = mapM_ (\i -> writePrimArray array i a) [from .. from + count - 1]

getSizeofMutablePrimArray :: MutablePrimArray s a -> ST s Int
getSizeofMutablePrimArray (MutablePrimArray elements) = pure (A.length elements)
