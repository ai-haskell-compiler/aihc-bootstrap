module Data.Monoid.Null where

-- From the package monoid-subclasses: monoids that can say whether a value
-- is mempty. Only the instances AIHC uses.

import Data.Map.Base (Map)
import qualified Data.Map.Base as Map
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Prelude hiding (null)
import qualified Prelude as P

class Monoid m => MonoidNull m where
  null :: m -> Bool

instance (Eq a, Num a) => MonoidNull (Sum a) where
  null (Sum a) = a == 0

instance MonoidNull [a] where
  null = P.null

instance Ord a => MonoidNull (Set a) where
  null = Set.null

instance Ord k => MonoidNull (Map k a) where
  null = Map.null
