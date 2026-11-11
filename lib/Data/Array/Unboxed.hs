module Data.Array.Unboxed (module Data.Array.Unboxed, module Data.Array) where

-- Nothing is unboxed in the interpreter: an unboxed array is an array.

import Data.Array

type UArray = Array
