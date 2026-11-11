module Data.Map.Strict (module Data.Map.Strict, module Data.Map.Base) where

-- The same map as Data.Map.Base. The functions that store values are
-- defined again here, to evaluate each value before it goes into the map.

import qualified Data.Map.Base as L
import Data.Map.Base (Map)
import Data.Set (Set)
import Prelude hiding (map)

-- The map, after evaluating every value in it.
strict :: Map k a -> Map k a
strict m = seq (L.foldr seq () m) m

singleton :: k -> a -> Map k a
singleton k a = seq a (L.singleton k a)

insert :: Ord k => k -> a -> Map k a -> Map k a
insert key value m = seq value (L.insert key value m)

insertWith :: Ord k => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f = insertWithKey (\_ new old -> f new old)

insertWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f key value m = case L.lookup key m of
  Nothing -> insert key value m
  Just old -> insert key (f key value old) m

insertLookupWithKey :: Ord k => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f key value m = (L.lookup key m, insertWithKey f key value m)

adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust f = alter (fmap f)

update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f = alter (\found -> found >>= f)

alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f key m = case f (L.lookup key m) of
  Nothing -> L.delete key m
  Just value -> insert key value m

findWithDefault :: Ord k => a -> k -> Map k a -> a
findWithDefault fallback key m = seq fallback (L.findWithDefault fallback key m)

fromList :: Ord k => [(k, a)] -> Map k a
fromList = foldl' (\m (k, a) -> insert k a m) L.empty

fromListWith :: Ord k => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith f = fromListWithKey (\_ new old -> f new old)

fromListWithKey :: Ord k => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey f = foldl' (\m (k, a) -> insertWithKey f k a m) L.empty

fromAscList :: Ord k => [(k, a)] -> Map k a
fromAscList = fromList

fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList pairs = strict (L.fromDistinctAscList pairs)

fromSet :: (k -> a) -> Set k -> Map k a
fromSet f keys = strict (L.fromSet f keys)

map :: (a -> b) -> Map k a -> Map k b
map f m = strict (L.map f m)

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey f m = strict (L.mapWithKey f m)

mapMaybe :: (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f m = strict (L.mapMaybe f m)

mapMaybeWithKey :: (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey f m = strict (L.mapMaybeWithKey f m)

mapKeysWith :: Ord k2 => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith combine f m = strict (L.mapKeysWith combine f m)

unionWith :: Ord k => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f left right = strict (L.unionWith f left right)

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f left right = strict (L.unionWithKey f left right)

unionsWith :: Ord k => (a -> a -> a) -> [Map k a] -> Map k a
unionsWith f maps = strict (L.unionsWith f maps)

intersectionWith :: Ord k => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f left right = strict (L.intersectionWith f left right)

intersectionWithKey :: Ord k => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey f left right = strict (L.intersectionWithKey f left right)

differenceWith :: Ord k => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f left right = strict (L.differenceWith f left right)

traverseWithKey :: Applicative t => (k -> a -> t b) -> Map k a -> t (Map k b)
traverseWithKey f m = fmap strict (L.traverseWithKey f m)
