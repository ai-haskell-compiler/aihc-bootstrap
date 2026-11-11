module Data.Traversable (module Data.Traversable, module Prelude) where

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for container f = traverse f container

forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
forM container f = mapM f container
