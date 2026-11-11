module Data.MonoidMap where

-- A map that has a value for every key: mempty, unless another value was
-- set. Only the values that are not mempty are stored, so two maps that
-- answer the same for every key are equal, and toList gives exactly the
-- keys with a value other than mempty, in ascending order. That is the
-- behaviour of the package monoidmap, which AIHC relies on.

import Control.DeepSeq (NFData)
import Data.Group (Group (..))
import Data.Map.Base (Map)
import qualified Data.Map.Base as Map
import Data.Monoid.Null (MonoidNull)
import qualified Data.Monoid.Null as Null
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (filter, map, null)

newtype MonoidMap k v = MonoidMap (Map k v)
  deriving (Eq, Show)

instance NFData (MonoidMap k v)

instance (Ord k, MonoidNull v) => Semigroup (MonoidMap k v) where
  (<>) = unionWith (<>)

instance (Ord k, MonoidNull v) => Monoid (MonoidMap k v) where
  mempty = empty

instance (Ord k, MonoidNull v) => MonoidNull (MonoidMap k v) where
  null = null

instance (Ord k, MonoidNull v, Group v) => Group (MonoidMap k v) where
  invert = map invert
  (~~) = minus

instance Foldable (MonoidMap k) where
  foldr f end (MonoidMap m) = Map.foldr f end m

-- Construction

empty :: MonoidMap k v
empty = MonoidMap Map.empty

-- Values with the same key are combined, the earlier one on the left.
fromList :: (Ord k, MonoidNull v) => [(k, v)] -> MonoidMap k v
fromList = fromListWith (<>)

fromListWith :: (Ord k, MonoidNull v) => (v -> v -> v) -> [(k, v)] -> MonoidMap k v
fromListWith f list = fromMap (Map.fromListWith (flip f) list)

fromMap :: MonoidNull v => Map k v -> MonoidMap k v
fromMap m = MonoidMap (Map.filter (not . Null.null) m)

fromMapWith :: MonoidNull v2 => (v1 -> v2) -> Map k v1 -> MonoidMap k v2
fromMapWith f m = fromMap (Map.map f m)

fromSet :: MonoidNull v => (k -> v) -> Set k -> MonoidMap k v
fromSet f keys = fromMap (Map.fromSet f keys)

singleton :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v
singleton k v = set k v empty

-- Deconstruction

toList :: MonoidMap k v -> [(k, v)]
toList (MonoidMap m) = Map.toAscList m

toMap :: MonoidMap k v -> Map k v
toMap (MonoidMap m) = m

-- One key

get :: (Ord k, Monoid v) => k -> MonoidMap k v -> v
get k (MonoidMap m) = Map.findWithDefault mempty k m

set :: (Ord k, MonoidNull v) => k -> v -> MonoidMap k v -> MonoidMap k v
set k v (MonoidMap m)
  | Null.null v = MonoidMap (Map.delete k m)
  | otherwise = MonoidMap (Map.insert k v m)

adjust :: (Ord k, MonoidNull v) => (v -> v) -> k -> MonoidMap k v -> MonoidMap k v
adjust f k m = set k (f (get k m)) m

nullify :: Ord k => k -> MonoidMap k v -> MonoidMap k v
nullify k (MonoidMap m) = MonoidMap (Map.delete k m)

nullifyKeysIn :: Ord k => Set k -> MonoidMap k v -> MonoidMap k v
nullifyKeysIn keys (MonoidMap m) = MonoidMap (Map.withoutKeys m keys)

nullifyKeysNotIn :: Ord k => Set k -> MonoidMap k v -> MonoidMap k v
nullifyKeysNotIn keys (MonoidMap m) = MonoidMap (Map.restrictKeys m keys)

-- Questions

null :: MonoidMap k v -> Bool
null (MonoidMap m) = Map.null m

nullKey :: Ord k => k -> MonoidMap k v -> Bool
nullKey k (MonoidMap m) = Map.notMember k m

nonNull :: MonoidMap k v -> Bool
nonNull m = not (null m)

nonNullCount :: MonoidMap k v -> Int
nonNullCount (MonoidMap m) = Map.size m

nonNullKey :: Ord k => k -> MonoidMap k v -> Bool
nonNullKey k (MonoidMap m) = Map.member k m

nonNullKeys :: MonoidMap k v -> Set k
nonNullKeys (MonoidMap m) = Map.keysSet m

-- Every value

filter :: (v -> Bool) -> MonoidMap k v -> MonoidMap k v
filter f (MonoidMap m) = MonoidMap (Map.filter f m)

filterWithKey :: (k -> v -> Bool) -> MonoidMap k v -> MonoidMap k v
filterWithKey f (MonoidMap m) = MonoidMap (Map.filterWithKey f m)

-- The function is only applied to the values that are not mempty.
map :: MonoidNull v2 => (v1 -> v2) -> MonoidMap k v1 -> MonoidMap k v2
map f (MonoidMap m) = fromMap (Map.map f m)

mapWithKey :: MonoidNull v2 => (k -> v1 -> v2) -> MonoidMap k v1 -> MonoidMap k v2
mapWithKey f (MonoidMap m) = fromMap (Map.mapWithKey f m)

-- Two maps

-- The function is applied at every key where either map has a value other
-- than mempty.
unionWith ::
  (Ord k, Monoid v1, Monoid v2, MonoidNull v3) =>
  (v1 -> v2 -> v3) ->
  MonoidMap k v1 ->
  MonoidMap k v2 ->
  MonoidMap k v3
unionWith f m1 m2 =
  fromSet (\k -> f (get k m1) (get k m2)) (Set.union (nonNullKeys m1) (nonNullKeys m2))

append :: (Ord k, MonoidNull v) => MonoidMap k v -> MonoidMap k v -> MonoidMap k v
append = unionWith (<>)

minus :: (Ord k, MonoidNull v, Group v) => MonoidMap k v -> MonoidMap k v -> MonoidMap k v
minus = unionWith (~~)
