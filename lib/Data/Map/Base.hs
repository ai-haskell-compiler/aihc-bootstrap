module Data.Map.Base where

-- Data.Map: a weight-balanced search tree, as in the containers package but
-- with the simplest algorithm for each operation. The shape of a tree can
-- differ from containers; nothing AIHC uses can see the shape, and the
-- order of every result follows from the keys.
--
-- This module is the lazy map. Data.Map.Strict adds the functions that
-- evaluate values as they are stored.

import Control.DeepSeq (NFData)
import Data.Set (Set (..))
import Prelude hiding (filter, foldr, lookup, map, null)
import qualified Prelude as P

infixl 9 !

data Map k a
  = Tip
  | -- The number of elements, a key and its value, and the maps of the
    -- smaller and the larger keys.
    Bin Int k a (Map k a) (Map k a)

-- ---- The tree ----

size :: Map k a -> Int
size Tip = 0
size (Bin count _ _ _ _) = count

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k a left right = Bin (size left + size right + 1) k a left right

-- A node whose sides may be out of balance by one insertion or deletion.
-- Neither side may be more than three times the size of the other.
balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k a left right
  | size left + size right <= 1 = bin k a left right
  | size right > 3 * size left = rotateLeft k a left right
  | size left > 3 * size right = rotateRight k a left right
  | otherwise = bin k a left right

rotateLeft :: k -> a -> Map k a -> Map k a -> Map k a
rotateLeft k a left (Bin _ k2 a2 middle right)
  | size middle < 2 * size right = bin k2 a2 (bin k a left middle) right
  | otherwise = case middle of
      Bin _ k3 a3 middleLeft middleRight ->
        bin k3 a3 (bin k a left middleLeft) (bin k2 a2 middleRight right)
      Tip -> bin k2 a2 (bin k a left middle) right
rotateLeft k a left Tip = bin k a left Tip

rotateRight :: k -> a -> Map k a -> Map k a -> Map k a
rotateRight k a (Bin _ k2 a2 left middle) right
  | size middle < 2 * size left = bin k2 a2 left (bin k a middle right)
  | otherwise = case middle of
      Bin _ k3 a3 middleLeft middleRight ->
        bin k3 a3 (bin k2 a2 left middleLeft) (bin k a middleRight right)
      Tip -> bin k2 a2 left (bin k a middle right)
rotateRight k a Tip right = bin k a Tip right

-- The smallest key with its value, and the map without it.
minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey Tip = Nothing
minViewWithKey (Bin _ k a Tip right) = Just ((k, a), right)
minViewWithKey (Bin _ k a left right) = case minViewWithKey left of
  Just (smallest, rest) -> Just (smallest, balance k a rest right)
  Nothing -> Nothing

maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey Tip = Nothing
maxViewWithKey (Bin _ k a left Tip) = Just ((k, a), left)
maxViewWithKey (Bin _ k a left right) = case maxViewWithKey right of
  Just (largest, rest) -> Just (largest, balance k a left rest)
  Nothing -> Nothing

-- Two maps as one, where every key of the first is smaller than every key
-- of the second, and the two are in balance with each other.
glue :: Map k a -> Map k a -> Map k a
glue Tip right = right
glue left Tip = left
glue left right =
  if size left > size right
    then case maxViewWithKey left of
      Just ((k, a), rest) -> balance k a rest right
      Nothing -> right
    else case minViewWithKey right of
      Just ((k, a), rest) -> balance k a left rest
      Nothing -> left

-- ---- Construction ----

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k a = Bin 1 k a Tip Tip

insert :: Ord k => k -> a -> Map k a -> Map k a
insert key value Tip = singleton key value
insert key value (Bin count k a left right) = case compare key k of
  LT -> balance k a (insert key value left) right
  GT -> balance k a left (insert key value right)
  EQ -> Bin count key value left right

-- insertWith f key new: f new old if the key is there already.
insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f = insertWithKey (\_ new old -> f new old)

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey _ key value Tip = singleton key value
insertWithKey f key value (Bin count k a left right) = case compare key k of
  LT -> balance k a (insertWithKey f key value left) right
  GT -> balance k a left (insertWithKey f key value right)
  EQ -> Bin count key (f key value a) left right

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f key value m = (lookup key m, insertWithKey f key value m)

delete :: Ord k => k -> Map k a -> Map k a
delete _ Tip = Tip
delete key (Bin _ k a left right) = case compare key k of
  LT -> balance k a (delete key left) right
  GT -> balance k a left (delete key right)
  EQ -> glue left right

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = alter (fmap f)

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = alter (\found -> found >>= f)

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f key m = case f (lookup key m) of
  Nothing -> delete key m
  Just value -> insert key value m

fromList :: Ord k => [(k, a)] -> Map k a
fromList = foldl' (\m (k, a) -> insert k a m) empty

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith f = fromListWithKey (\_ new old -> f new old)

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey f = foldl' (\m (k, a) -> insertWithKey f k a m) empty

fromAscList :: Ord k => [(k, a)] -> Map k a
fromAscList = fromList

-- From keys that are in order and all different: a balanced tree, built
-- without comparing anything.
fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList pairs = fst (build (length pairs) pairs)
  where
    build count list =
      if count == 0
        then (Tip, list)
        else case build (quot count 2) list of
          (left, (k, a) : rest) -> case build (count - quot count 2 - 1) rest of
            (right, remaining) -> (bin k a left right, remaining)
          (left, []) -> (left, [])

fromSet :: (k -> a) -> Set k -> Map k a
fromSet f (Set keys) = mapWithKey (\k _ -> f k) keys

-- ---- Queries ----

null :: Map k a -> Bool
null Tip = True
null _ = False

lookup :: Ord k => k -> Map k a -> Maybe a
lookup _ Tip = Nothing
lookup key (Bin _ k a left right) = case compare key k of
  LT -> lookup key left
  GT -> lookup key right
  EQ -> Just a

member :: Ord k => k -> Map k a -> Bool
member key m = case lookup key m of
  Nothing -> False
  Just _ -> True

notMember :: Ord k => k -> Map k a -> Bool
notMember key m = not (member key m)

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault fallback key m = case lookup key m of
  Nothing -> fallback
  Just a -> a

(!) :: Ord k => Map k a -> k -> a
(!) m key = case lookup key m of
  Nothing -> error "Map.!: given key is not an element in the map"
  Just a -> a

(!?) :: Ord k => Map k a -> k -> Maybe a
(!?) m key = lookup key m

-- The entry with the largest key that is smaller than the given one, and
-- the three functions like it.
lookupLT :: Ord k => k -> Map k a -> Maybe (k, a)
lookupLT key m = lastOf (P.filter (\(k, _) -> k < key) (toAscList m))

lookupLE :: Ord k => k -> Map k a -> Maybe (k, a)
lookupLE key m = lastOf (P.filter (\(k, _) -> k <= key) (toAscList m))

lookupGT :: Ord k => k -> Map k a -> Maybe (k, a)
lookupGT key m = firstOf (P.filter (\(k, _) -> k > key) (toAscList m))

lookupGE :: Ord k => k -> Map k a -> Maybe (k, a)
lookupGE key m = firstOf (P.filter (\(k, _) -> k >= key) (toAscList m))

firstOf :: [a] -> Maybe a
firstOf [] = Nothing
firstOf (a : _) = Just a

lastOf :: [a] -> Maybe a
lastOf [] = Nothing
lastOf list = Just (last list)

lookupMin :: Map k a -> Maybe (k, a)
lookupMin m = fmap fst (minViewWithKey m)

lookupMax :: Map k a -> Maybe (k, a)
lookupMax m = fmap fst (maxViewWithKey m)

findMin :: Map k a -> (k, a)
findMin m = case lookupMin m of
  Just entry -> entry
  Nothing -> error "Map.findMin: empty map has no minimal element"

findMax :: Map k a -> (k, a)
findMax m = case lookupMax m of
  Just entry -> entry
  Nothing -> error "Map.findMax: empty map has no maximal element"

deleteMin :: Map k a -> Map k a
deleteMin m = case minViewWithKey m of
  Just (_, rest) -> rest
  Nothing -> Tip

deleteMax :: Map k a -> Map k a
deleteMax m = case maxViewWithKey m of
  Just (_, rest) -> rest
  Nothing -> Tip

minView :: Map k a -> Maybe (a, Map k a)
minView m = fmap (\((_, a), rest) -> (a, rest)) (minViewWithKey m)

maxView :: Map k a -> Maybe (a, Map k a)
maxView m = fmap (\((_, a), rest) -> (a, rest)) (maxViewWithKey m)

-- ---- Folds and lists ----

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey _ end Tip = end
foldrWithKey f end (Bin _ k a left right) =
  foldrWithKey f (f k a (foldrWithKey f end right)) left

foldlWithKey :: (b -> k -> a -> b) -> b -> Map k a -> b
foldlWithKey _ start Tip = start
foldlWithKey f start (Bin _ k a left right) =
  foldlWithKey f (f (foldlWithKey f start left) k a) right

foldr :: (a -> b -> b) -> b -> Map k a -> b
foldr f = foldrWithKey (\_ a rest -> f a rest)

foldl :: (b -> a -> b) -> b -> Map k a -> b
foldl f = foldlWithKey (\total _ a -> f total a)

foldr' :: (a -> b -> b) -> b -> Map k a -> b
foldr' = foldr

foldlWithKey' :: (b -> k -> a -> b) -> b -> Map k a -> b
foldlWithKey' f start m = P.foldl' (\total (k, a) -> f total k a) start (toAscList m)

foldrWithKey' :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey' = foldrWithKey

toAscList :: Map k a -> [(k, a)]
toAscList = foldrWithKey (\k a rest -> (k, a) : rest) []

toDescList :: Map k a -> [(k, a)]
toDescList = foldlWithKey (\rest k a -> (k, a) : rest) []

toList :: Map k a -> [(k, a)]
toList = toAscList

assocs :: Map k a -> [(k, a)]
assocs = toAscList

keys :: Map k a -> [k]
keys = foldrWithKey (\k _ rest -> k : rest) []

elems :: Map k a -> [a]
elems = foldr (:) []

keysSet :: Map k a -> Set k
keysSet m = Set (map (\_ -> ()) m)

-- ---- Maps over maps ----

map :: (a -> b) -> Map k a -> Map k b
map f = mapWithKey (\_ a -> f a)

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ Tip = Tip
mapWithKey f (Bin count k a left right) =
  Bin count k (f k a) (mapWithKey f left) (mapWithKey f right)

-- The keys are visited in order.
traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey _ Tip = pure Tip
traverseWithKey f (Bin count k a left right) =
  liftA2
    (\newLeft (b, newRight) -> Bin count k b newLeft newRight)
    (traverseWithKey f left)
    (liftA2 (\b newRight -> (b, newRight)) (f k a) (traverseWithKey f right))

-- If two keys become the same, the value of the larger one is kept.
mapKeys :: Ord k2 => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys f m = fromList (P.map (\(k, a) -> (f k, a)) (toAscList m))

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith combine f m = fromListWith combine (P.map (\(k, a) -> (f k, a)) (toAscList m))

filter :: (a -> Bool) -> Map k a -> Map k a
filter keep = filterWithKey (\_ a -> keep a)

filterWithKey :: (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey keep m = fromDistinctAscList (P.filter (\(k, a) -> keep k a) (toAscList m))

partition :: (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition keep = partitionWithKey (\_ a -> keep a)

partitionWithKey :: (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey keep m = (filterWithKey keep m, filterWithKey (\k a -> not (keep k a)) m)

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f = mapMaybeWithKey (\_ a -> f a)

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f m = fromDistinctAscList (pick (toAscList m))
  where
    pick [] = []
    pick ((k, a) : rest) = case f k a of
      Nothing -> pick rest
      Just b -> (k, b) : pick rest

mapEither :: (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m = (mapMaybe (\a -> either Just (\_ -> Nothing) (f a)) m, mapMaybe (\a -> either (\_ -> Nothing) Just (f a)) m)

-- ---- Combining maps ----

-- Of a key that both maps have, the first map's value is kept.
union :: Ord k => Map k a -> Map k a -> Map k a
union = unionWith const

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f = unionWithKey (\_ a b -> f a b)

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f left right = foldlWithKey (\m k a -> insertWithKey f k a m) right left

unions :: Ord k => [Map k a] -> Map k a
unions = P.foldl' union empty

unionsWith :: Ord k => (a -> a -> a) -> [Map k a] -> Map k a
unionsWith f = P.foldl' (unionWith f) empty

difference :: Ord k => Map k a -> Map k b -> Map k a
difference left right = filterWithKey (\k _ -> notMember k right) left

(\\) :: Ord k => Map k a -> Map k b -> Map k a
(\\) = difference

differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f left right = mapMaybeWithKey keep left
  where
    keep k a = case lookup k right of
      Nothing -> Just a
      Just b -> f a b

intersection :: Ord k => Map k a -> Map k b -> Map k a
intersection left right = filterWithKey (\k _ -> member k right) left

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f = intersectionWithKey (\_ a b -> f a b)

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey f left right = mapMaybeWithKey (\k a -> fmap (f k a) (lookup k right)) left

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
restrictKeys m (Set keep) = filterWithKey (\k _ -> member k keep) m

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
withoutKeys m (Set drop) = filterWithKey (\k _ -> notMember k drop) m

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf left right = all (\(k, a) -> lookup k right == Just a) (toAscList left)

disjoint :: Ord k => Map k a -> Map k b -> Bool
disjoint left right = null (intersection left right)

-- ---- Instances ----

instance (Eq k, Eq a) => Eq (Map k a) where
  (==) left right = size left == size right && toAscList left == toAscList right

instance (Ord k, Ord a) => Ord (Map k a) where
  compare left right = compare (toAscList left) (toAscList right)

instance (Show k, Show a) => Show (Map k a) where
  showsPrec precedence m =
    showParen (precedence > 10) (\rest -> "fromList " ++ shows (toAscList m) rest)

instance Functor (Map k) where
  fmap = map

instance Foldable (Map k) where
  foldr = foldr

instance Traversable (Map k) where
  traverse f = traverseWithKey (\_ a -> f a)

instance Ord k => Semigroup (Map k a) where
  (<>) = union

instance Ord k => Monoid (Map k a) where
  mempty = empty

instance NFData (Map k a)
