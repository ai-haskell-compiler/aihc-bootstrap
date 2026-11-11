module Data.Traversable (module Data.Traversable, module Prelude, forM) where

import Control.Monad (forM)

for :: (Traversable t, Applicative f) => t a -> (a -> f b) -> f (t b)
for container f = traverse f container
