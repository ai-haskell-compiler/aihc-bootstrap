module Data.Vector.Unboxed.Mutable
  ( module Data.Vector.Unboxed.Mutable,
    module Data.Vector.Mutable,
  )
where

-- Nothing is unboxed in the interpreter: an unboxed vector is a vector.
-- One thing differs from a boxed one: a new unboxed vector is all zero.

import Control.Monad.ST (ST)
import Data.Int
import Data.Vector.Mutable hiding (new, unsafeNew)
import qualified Data.Vector.Mutable as G
import Data.Word

-- The types that can be in an unboxed vector, with the value that is all
-- zero bits.
class Unbox a where
  zero :: a

instance Unbox Int where
  zero = 0

instance Unbox Int8 where
  zero = 0

instance Unbox Int16 where
  zero = 0

instance Unbox Int32 where
  zero = 0

instance Unbox Int64 where
  zero = 0

instance Unbox Word where
  zero = 0

instance Unbox Word8 where
  zero = 0

instance Unbox Word16 where
  zero = 0

instance Unbox Word32 where
  zero = 0

instance Unbox Word64 where
  zero = 0

instance Unbox Bool where
  zero = False

instance Unbox Char where
  zero = '\0'

instance Unbox () where
  zero = ()

instance (Unbox a, Unbox b) => Unbox (a, b) where
  zero = (zero, zero)

instance (Unbox a, Unbox b, Unbox c) => Unbox (a, b, c) where
  zero = (zero, zero, zero)

new :: Unbox a => Int -> ST s (MVector s a)
new count = G.replicate count zero

-- The elements are not to be read before they are written.
unsafeNew :: Int -> ST s (MVector s a)
unsafeNew = G.unsafeNew
