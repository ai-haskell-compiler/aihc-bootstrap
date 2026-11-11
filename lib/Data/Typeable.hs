module Data.Typeable where

-- Every type is known when the program runs, so nothing has to be an
-- instance of this class: it is only there to be named.

class Typeable a

-- The value at another type, if that is the type it has.
foreign import prim "cast" cast :: a -> Maybe b
