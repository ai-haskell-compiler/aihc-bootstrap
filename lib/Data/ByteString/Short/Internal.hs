module Data.ByteString.Short.Internal where

-- A short byte string is a byte string here. Upstream SBS holds an unboxed
-- array, the one that Data.Primitive.ByteArray.ByteArray holds too; here
-- both hold a ByteString.

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

newtype ShortByteString = SBS ByteString

instance Eq ShortByteString where
  (==) (SBS a) (SBS b) = a == b

instance Ord ShortByteString where
  compare (SBS a) (SBS b) = compare a b

instance Show ShortByteString where
  showsPrec precedence (SBS bytes) = showsPrec precedence bytes

toShort :: ByteString -> ShortByteString
toShort = SBS

fromShort :: ShortByteString -> ByteString
fromShort (SBS bytes) = bytes

length :: ShortByteString -> Int
length (SBS bytes) = B.length bytes
