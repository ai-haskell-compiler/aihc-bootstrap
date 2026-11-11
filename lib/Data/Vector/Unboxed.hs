module Data.Vector.Unboxed (module Data.Vector, Unbox) where

-- Nothing is unboxed in the interpreter: an unboxed vector is a vector.

import Data.Vector
import Data.Vector.Unboxed.Mutable (Unbox)
