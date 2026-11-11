module Data.Primitive.ByteArray where

-- Bytes that are changed in place, in ST. They are a ByteString whose
-- bytes the builtins of src/bytes.c overwrite; once frozen, nothing does.
--
-- Upstream reads and writes any primitive type at an index counted in
-- elements of that type. AIHC reads and writes single bytes, so here the
-- element is Word8.

import Control.Monad.ST (ST (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

newtype MutableByteArray s = MutableByteArray ByteString

-- Upstream this holds an unboxed array. What matters is that it is the
-- same thing that Data.ByteString.Short.Internal.SBS holds.
data ByteArray = ByteArray ByteString

-- All bytes are zero.
newByteArray :: Int -> ST s (MutableByteArray s)
newByteArray count = ST (fmap MutableByteArray (bytesNew count))

newPinnedByteArray :: Int -> ST s (MutableByteArray s)
newPinnedByteArray = newByteArray

readByteArray :: MutableByteArray s -> Int -> ST s Word8
readByteArray (MutableByteArray bytes) i = ST (pure (B.index bytes i) >>= \byte -> seq byte (pure byte))

writeByteArray :: MutableByteArray s -> Int -> Word8 -> ST s ()
writeByteArray (MutableByteArray bytes) i byte = ST (bytesPoke bytes i byte)

indexByteArray :: ByteArray -> Int -> Word8
indexByteArray (ByteArray bytes) i = B.index bytes i

sizeofByteArray :: ByteArray -> Int
sizeofByteArray (ByteArray bytes) = B.length bytes

getSizeofMutableByteArray :: MutableByteArray s -> ST s Int
getSizeofMutableByteArray (MutableByteArray bytes) = pure (B.length bytes)

-- copyByteArray target at source from count
copyByteArray :: MutableByteArray s -> Int -> ByteArray -> Int -> Int -> ST s ()
copyByteArray (MutableByteArray target) at (ByteArray source) from count =
  ST (bytesBlit target at source from count)

-- As copyByteArray, between mutable arrays, which may be the same one.
moveByteArray :: MutableByteArray s -> Int -> MutableByteArray s -> Int -> Int -> ST s ()
moveByteArray (MutableByteArray target) at (MutableByteArray source) from count =
  ST (bytesBlit target at source from count)

copyMutableByteArray :: MutableByteArray s -> Int -> MutableByteArray s -> Int -> Int -> ST s ()
copyMutableByteArray = moveByteArray

-- The same bytes, on the promise that they are not changed any more.
unsafeFreezeByteArray :: MutableByteArray s -> ST s ByteArray
unsafeFreezeByteArray (MutableByteArray bytes) = pure (ByteArray bytes)

foreign import prim "bytesNew" bytesNew :: Int -> IO ByteString

foreign import prim "bytesPoke" bytesPoke :: ByteString -> Int -> Word8 -> IO ()

foreign import prim "bytesBlit" bytesBlit :: ByteString -> Int -> ByteString -> Int -> Int -> IO ()
