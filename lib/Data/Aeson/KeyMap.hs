module Data.Aeson.KeyMap where

-- The fields of a JSON object, sorted by name.

import Data.Aeson.Key (Key)
import Data.Map.Base (Map)
import qualified Data.Map.Base as Map
import Prelude hiding (lookup, null)

newtype KeyMap v = KeyMap (Map Key v)

instance Eq v => Eq (KeyMap v) where
  (==) (KeyMap a) (KeyMap b) = a == b

instance Show v => Show (KeyMap v) where
  showsPrec precedence (KeyMap m) = showsPrec precedence m

instance Functor KeyMap where
  fmap f (KeyMap m) = KeyMap (Map.map f m)

empty :: KeyMap v
empty = KeyMap Map.empty

null :: KeyMap v -> Bool
null (KeyMap m) = Map.null m

size :: KeyMap v -> Int
size (KeyMap m) = Map.size m

singleton :: Key -> v -> KeyMap v
singleton key v = KeyMap (Map.singleton key v)

insert :: Key -> v -> KeyMap v -> KeyMap v
insert key v (KeyMap m) = KeyMap (Map.insert key v m)

delete :: Key -> KeyMap v -> KeyMap v
delete key (KeyMap m) = KeyMap (Map.delete key m)

lookup :: Key -> KeyMap v -> Maybe v
lookup key (KeyMap m) = Map.lookup key m

member :: Key -> KeyMap v -> Bool
member key (KeyMap m) = Map.member key m

-- A later value of a key replaces an earlier one.
fromList :: [(Key, v)] -> KeyMap v
fromList pairs = KeyMap (Map.fromList pairs)

toList :: KeyMap v -> [(Key, v)]
toList (KeyMap m) = Map.toAscList m

keys :: KeyMap v -> [Key]
keys (KeyMap m) = Map.keys m

elems :: KeyMap v -> [v]
elems (KeyMap m) = Map.elems m
