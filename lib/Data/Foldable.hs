module Data.Foldable (module Prelude) where

import Data.List (find)

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldl' (\total a -> total <> f a) mempty

for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
for_ container f = traverse_ f container

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ container f = mapM_ f container

sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (\action rest -> action *> rest) (pure ())

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f start container = foldl (\continue a total -> f a total >>= continue) pure container start

foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f start container = foldr (\a continue total -> f total a >>= continue) pure container start
