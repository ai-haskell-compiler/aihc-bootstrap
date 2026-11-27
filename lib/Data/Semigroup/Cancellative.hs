module Data.Semigroup.Cancellative where

-- The one class of the package monoid-subclasses that AIHC needs, through
-- monoidmap's isSubmapOf, with its instance for sets.

import Data.Set (Set)
import qualified Data.Set as Set

class Semigroup m => Reductive m where
  -- What is left of the first after taking the second away, if the second
  -- is a part of it.
  (</>) :: m -> m -> Maybe m

instance Ord a => Reductive (Set a) where
  a </> b = if Set.isSubsetOf b a then Just (Set.difference a b) else Nothing
