module Data.Coerce where

-- Newtypes have no representation of their own in the interpreter, so a
-- coercion does nothing.
foreign import prim "unsafeCoerce" coerce :: a -> b
