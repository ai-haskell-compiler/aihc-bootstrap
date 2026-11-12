module Data.Typeable where

-- Every type is known when the program runs, so nothing has to be an
-- instance of this class: it is only there to be named.

class Typeable a

-- The value at another type, if that is the type it has.
foreign import prim "cast" cast :: a -> Maybe b

-- What eqT gives when two types are the same type. Upstream's type is
-- written a :~: b, and its Refl says in its type that a is b. Here the type
-- has an ordinary name, since AIHC never writes it, and that a is b is
-- something the type pass knows about a match on Refl (given equalities,
-- in src/infer.c).
data Same a b = Refl

-- Whether a and b are the same type.
eqT :: forall a b. Maybe (Same a b)
eqT = cast (Refl :: Same a a)
