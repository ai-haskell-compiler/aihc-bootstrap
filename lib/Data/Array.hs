module Data.Array where

-- An array is its bounds and its elements.
--
-- Upstream indexes with the class Ix. AIHC only indexes with Int, so here
-- an index is anything that can be enumerated: the element at index i is
-- the one at position fromEnum i - fromEnum low.

import Interpreter.Array (MutableArray)
import qualified Interpreter.Array as A
import System.IO.Unsafe (unsafePerformIO)

infixl 9 !

data Array i e = Array (i, i) (MutableArray e)

-- How many elements an array with those bounds has.
rangeSize :: Enum i => (i, i) -> Int
rangeSize (low, high) = max 0 (fromEnum high - fromEnum low + 1)

position :: Enum i => (i, i) -> i -> Int
position (low, _) i = fromEnum i - fromEnum low

array :: Enum i => (i, i) -> [(i, e)] -> Array i e
array limits associations = accumArray (\_ e -> e) A.uninitialised limits associations

listArray :: Enum i => (i, i) -> [e] -> Array i e
listArray limits list = Array limits (A.fromListN (rangeSize limits) list)

-- Every index starts at `initial`; then each (index, value) of the list is
-- combined into the element at its index, in the order of the list.
accumArray :: Enum i => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
accumArray combine initial limits associations = unsafePerformIO (do
  elements <- A.new (rangeSize limits) initial
  mapM_
    (\(i, a) -> A.read elements (position limits i) >>= \e -> A.write elements (position limits i) (combine e a))
    associations
  pure (Array limits elements))

(!) :: Enum i => Array i e -> i -> e
(!) (Array limits elements) i = A.index elements (position limits i)

bounds :: Array i e -> (i, i)
bounds (Array limits _) = limits

indices :: Enum i => Array i e -> [i]
indices (Array (low, high) _) = [low .. high]

elems :: Array i e -> [e]
elems (Array _ elements) = A.toList elements

assocs :: Enum i => Array i e -> [(i, e)]
assocs a = zip (indices a) (elems a)

instance Functor (Array i) where
  fmap f (Array limits elements) = Array limits (A.fromListN (A.length elements) (map f (A.toList elements)))
