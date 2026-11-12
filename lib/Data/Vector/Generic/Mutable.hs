module Data.Vector.Generic.Mutable where

-- Functions that work on every kind of vector that is changed in place.
-- Upstream has a class of such vectors with an instance for each kind
-- (boxed, unboxed, ...). Here there is one kind, so the class only says
-- that a vector is that kind. It has the functions AIHC uses.
--
-- Upstream's class has the type of the elements as a second parameter:
-- MVector v a. The interpreter has no classes with two parameters, and it
-- does not look at the constraints of a signature, so a signature that
-- says MVector v a is fine where this class is used.

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as M
import Prelude hiding (length, replicate)

class MVector v where
  toVector :: v s a -> M.MVector s a
  fromVector :: M.MVector s a -> v s a

instance MVector M.MVector where
  toVector vector = vector
  fromVector vector = vector

length :: MVector v => v s a -> Int
length vector = M.length (toVector vector)

replicate :: MVector v => Int -> a -> ST s (v s a)
replicate count a = fmap fromVector (M.replicate count a)

unsafeRead :: MVector v => v s a -> Int -> ST s a
unsafeRead vector = M.unsafeRead (toVector vector)

unsafeWrite :: MVector v => v s a -> Int -> a -> ST s ()
unsafeWrite vector = M.unsafeWrite (toVector vector)

set :: MVector v => v s a -> a -> ST s ()
set vector = M.set (toVector vector)

unsafeSlice :: MVector v => Int -> Int -> v s a -> v s a
unsafeSlice from count vector = fromVector (M.unsafeSlice from count (toVector vector))

unsafeGrow :: MVector v => v s a -> Int -> ST s (v s a)
unsafeGrow vector more = fmap fromVector (M.unsafeGrow (toVector vector) more)
