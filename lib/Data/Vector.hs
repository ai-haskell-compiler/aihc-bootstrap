module Data.Vector where

-- A vector that is not changed: a part of an array.

import Control.Monad.ST (ST (..))
import Data.Vector.Mutable (MVector (..))
import Interpreter.Array (MutableArray)
import qualified Interpreter.Array as A
import Prelude hiding (length, null)
import qualified Prelude as P

infixl 9 !

data Vector a = Vector Int Int (MutableArray a)

empty :: Vector a
empty = fromList []

fromList :: [a] -> Vector a
fromList list = let count = P.length list in Vector 0 count (A.fromListN count list)

toList :: Vector a -> [a]
toList (Vector start count elements) = [A.index elements (start + i) | i <- [0 .. count - 1]]

length :: Vector a -> Int
length (Vector _ count _) = count

null :: Vector a -> Bool
null vector = length vector == 0

(!) :: Vector a -> Int -> a
(!) (Vector start count elements) i =
  if i < 0 || i >= count
    then error "Data.Vector.!: the index is out of range"
    else A.index elements (start + i)

(!?) :: Vector a -> Int -> Maybe a
(!?) vector i = if i < 0 || i >= length vector then Nothing else Just (vector ! i)

unsafeIndex :: Vector a -> Int -> a
unsafeIndex = (!)

-- A copy of the mutable vector that is not changed any more.
freeze :: MVector s a -> ST s (Vector a)
freeze (MVector start count elements) = ST (fmap (Vector 0 count) (A.clone elements start count))

thaw :: Vector a -> ST s (MVector s a)
thaw (Vector start count elements) = ST (fmap (MVector 0 count) (A.clone elements start count))

-- The same elements, on the promise that they are not changed any more.
unsafeFreeze :: MVector s a -> ST s (Vector a)
unsafeFreeze (MVector start count elements) = pure (Vector start count elements)

unsafeThaw :: Vector a -> ST s (MVector s a)
unsafeThaw (Vector start count elements) = pure (MVector start count elements)

instance Eq a => Eq (Vector a) where
  (==) a b = toList a == toList b

instance Show a => Show (Vector a) where
  showsPrec precedence vector = showsPrec precedence (toList vector)

instance Functor Vector where
  fmap f vector = fromList (map f (toList vector))

instance Foldable Vector where
  foldr f end vector = P.foldr f end (toList vector)
