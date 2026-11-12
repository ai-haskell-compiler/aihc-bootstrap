module Data.Vector.Mutable where

-- A vector that is changed in place: a part of an array. Upstream has
-- several kinds of such vectors (boxed, unboxed, ...) that work in any
-- monad with state; here there is one kind, in ST.

import Control.Monad.ST (ST (..))
import Interpreter.Array (MutableArray)
import qualified Interpreter.Array as A
import Prelude hiding (drop, foldr, length, read, replicate, take)
import qualified Prelude as P

-- Where the part starts, how long it is, and the array.
data MVector s a = MVector Int Int (MutableArray a)

length :: MVector s a -> Int
length (MVector _ count _) = count

null :: MVector s a -> Bool
null vector = length vector == 0

replicate :: Int -> a -> ST s (MVector s a)
replicate count a = ST (fmap (MVector 0 (max 0 count)) (A.new (max 0 count) a))

-- The elements are not to be read before they are written.
new :: Int -> ST s (MVector s a)
new count = replicate count A.uninitialised

unsafeNew :: Int -> ST s (MVector s a)
unsafeNew = new

read :: MVector s a -> Int -> ST s a
read (MVector start count elements) i =
  if i < 0 || i >= count
    then error "Data.Vector.Mutable.read: the index is out of range"
    else ST (A.read elements (start + i))

write :: MVector s a -> Int -> a -> ST s ()
write (MVector start count elements) i a =
  if i < 0 || i >= count
    then error "Data.Vector.Mutable.write: the index is out of range"
    else ST (A.write elements (start + i) a)

unsafeRead :: MVector s a -> Int -> ST s a
unsafeRead = read

unsafeWrite :: MVector s a -> Int -> a -> ST s ()
unsafeWrite = write

modify :: MVector s a -> (a -> a) -> Int -> ST s ()
modify vector f i = read vector i >>= \a -> write vector i (f a)

modify' :: MVector s a -> (a -> a) -> Int -> ST s ()
modify' vector f i = read vector i >>= \a -> let b = f a in seq b (write vector i b)

swap :: MVector s a -> Int -> Int -> ST s ()
swap vector i j = read vector i >>= \a -> read vector j >>= \b -> write vector i b >> write vector j a

-- Every element becomes the value.
set :: MVector s a -> a -> ST s ()
set vector a = mapM_ (\i -> write vector i a) [0 .. length vector - 1]

-- A part of the vector. It shares the elements.
slice :: Int -> Int -> MVector s a -> MVector s a
slice from count (MVector start _ elements) = MVector (start + from) count elements

unsafeSlice :: Int -> Int -> MVector s a -> MVector s a
unsafeSlice = slice

take :: Int -> MVector s a -> MVector s a
take n vector = slice 0 (max 0 (min n (length vector))) vector

drop :: Int -> MVector s a -> MVector s a
drop n vector = let k = max 0 (min n (length vector)) in slice k (length vector - k) vector

-- A new vector with the same elements and room for that many more, which
-- are not to be read before they are written.
grow :: MVector s a -> Int -> ST s (MVector s a)
grow (MVector start count elements) more = ST (do
  larger <- A.new (count + more) A.uninitialised
  A.copy elements start larger 0 count
  pure (MVector 0 (count + more) larger))

unsafeGrow :: MVector s a -> Int -> ST s (MVector s a)
unsafeGrow = grow

clone :: MVector s a -> ST s (MVector s a)
clone (MVector start count elements) = ST (fmap (MVector 0 count) (A.clone elements start count))

-- copy target source
copy :: MVector s a -> MVector s a -> ST s ()
copy (MVector to _ target) (MVector from count source) = ST (A.copy source from target to count)

unsafeCopy :: MVector s a -> MVector s a -> ST s ()
unsafeCopy = copy

toList :: MVector s a -> ST s [a]
toList vector = mapM (read vector) [0 .. length vector - 1]

foldr :: (a -> b -> b) -> b -> MVector s a -> ST s b
foldr f end vector = fmap (P.foldr f end) (toList vector)

foldl' :: (b -> a -> b) -> b -> MVector s a -> ST s b
foldl' f start vector = fmap (P.foldl' f start) (toList vector)
