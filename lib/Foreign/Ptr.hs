module Foreign.Ptr where

-- There is no memory to point into. A pointer is the bytes it would point
-- at, which is enough for the one thing AIHC does with one: hand the bytes
-- of a ByteString to a function that reads them.

import Data.ByteString (ByteString)

newtype Ptr a = Ptr ByteString

castPtr :: Ptr a -> Ptr b
castPtr (Ptr bytes) = Ptr bytes
