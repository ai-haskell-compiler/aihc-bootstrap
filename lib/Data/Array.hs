module Data.Array where

-- An array is its bounds and a map from each index to its element. That
-- makes (!) logarithmic instead of constant, which only costs time.
--
-- Upstream indexes with the class Ix. AIHC only indexes with Int, so here
-- an index is anything that can be ordered and enumerated.

import qualified Data.Map.Base as M

infixl 9 !

data Array i e = Array (i, i) (M.Map i e)

array :: Ord i => (i, i) -> [(i, e)] -> Array i e
array limits associations = Array limits (M.fromList associations)

listArray :: (Ord i, Enum i) => (i, i) -> [e] -> Array i e
listArray (low, high) list = array (low, high) (zip [low .. high] list)

-- Every index starts at `initial`; then each (index, value) of the list is
-- combined into the element at its index, in the order of the list.
accumArray :: (Ord i, Enum i) => (e -> a -> e) -> e -> (i, i) -> [(i, a)] -> Array i e
accumArray combine initial (low, high) associations =
  Array (low, high) (foldl add start associations)
  where
    start = M.fromList [(i, initial) | i <- [low .. high]]
    add elements (i, a) = M.adjust (\e -> combine e a) i elements

(!) :: Ord i => Array i e -> i -> e
(!) (Array _ elements) i = case M.lookup i elements of
  Just e -> e
  Nothing -> error "Data.Array.!: the index is out of range"

bounds :: Array i e -> (i, i)
bounds (Array limits _) = limits

indices :: Array i e -> [i]
indices (Array _ elements) = M.keys elements

elems :: Array i e -> [e]
elems (Array _ elements) = M.elems elements

assocs :: Array i e -> [(i, e)]
assocs (Array _ elements) = M.toList elements

instance Functor (Array i) where
  fmap f (Array limits elements) = Array limits (M.map f elements)
