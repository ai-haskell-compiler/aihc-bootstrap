module Data.ByteString.Unsafe where

-- The unchecked functions are the checked ones.

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Foreign.Ptr (Ptr (..))

unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex = B.index

unsafeHead :: ByteString -> Word8
unsafeHead = B.head

unsafeTail :: ByteString -> ByteString
unsafeTail = B.tail

unsafeTake :: Int -> ByteString -> ByteString
unsafeTake = B.take

unsafeDrop :: Int -> ByteString -> ByteString
unsafeDrop = B.drop

-- The bytes as a pointer and a length, for the action. See Foreign.Ptr.
unsafeUseAsCStringLen :: ByteString -> ((Ptr Char, Int) -> IO a) -> IO a
unsafeUseAsCStringLen bytes action = action (Ptr bytes, B.length bytes)
