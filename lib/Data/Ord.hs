module Data.Ord (module Prelude) where

comparing :: Ord b => (a -> b) -> a -> a -> Ordering
comparing f a b = compare (f a) (f b)

-- The same values in the opposite order.
newtype Down a = Down {getDown :: a}
  deriving (Eq, Show)

instance Ord a => Ord (Down a) where
  compare (Down a) (Down b) = compare b a

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)
