module Data.Group where

-- From the package groups: monoids where every value has an inverse. Only
-- the instance AIHC uses.

import Data.Monoid (Sum (..))

infixl 7 ~~

class Monoid m => Group m where
  invert :: m -> m
  (~~) :: m -> m -> m
  (~~) a b = a <> invert b

instance Num a => Group (Sum a) where
  invert (Sum a) = Sum (negate a)
