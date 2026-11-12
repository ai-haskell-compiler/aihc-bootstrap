module Data.Ix where

-- Upstream has a class of types that can index an array. AIHC only asks
-- whether a value is between two others.

inRange :: Ord a => (a, a) -> a -> Bool
inRange (low, high) a = low <= a && a <= high
