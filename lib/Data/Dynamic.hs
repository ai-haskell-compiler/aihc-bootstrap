module Data.Dynamic (module Data.Dynamic, Typeable) where

import Data.Typeable (Typeable, cast)

-- A value with its type, which the constructor keeps.
data Dynamic = forall a. Dynamic a

toDyn :: Typeable a => a -> Dynamic
toDyn = Dynamic

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic (Dynamic a) = cast a

fromDyn :: Typeable a => Dynamic -> a -> a
fromDyn dynamic fallback = case fromDynamic dynamic of
  Just a -> a
  Nothing -> fallback
