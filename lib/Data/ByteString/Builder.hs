module Data.ByteString.Builder where

-- A Builder is the list of byte strings it will produce, as a function that
-- puts them in front of whatever comes after. That makes (<>) cheap however
-- the builders are nested.

import Data.Bits (shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Int
import Data.Word

newtype Builder = Builder ([ByteString] -> [ByteString])

instance Semigroup Builder where
  (<>) (Builder first) (Builder second) = Builder (first . second)

instance Monoid Builder where
  mempty = Builder id

-- Lazy byte strings are strict ones here (Data.ByteString.Lazy).
toLazyByteString :: Builder -> ByteString
toLazyByteString (Builder chunks) = B.concat (chunks [])

byteString :: ByteString -> Builder
byteString bytes = Builder (bytes :)

lazyByteString :: ByteString -> Builder
lazyByteString = byteString

bytes :: [Word8] -> Builder
bytes list = byteString (B.pack list)

word8 :: Word8 -> Builder
word8 w = bytes [w]

int8 :: Int8 -> Builder
int8 i = word8 (fromIntegral i)

-- The lowest 8 bits of the character.
char8 :: Char -> Builder
char8 c = word8 (fromIntegral (ord c))

string8 :: String -> Builder
string8 string = bytes (map (fromIntegral . ord) string)

-- The lowest `count` bytes of the number, the lowest byte first.
littleEndian :: Int -> Word64 -> [Word8]
littleEndian count w = [fromIntegral (shiftR w (8 * i)) | i <- [0 .. count - 1]]

word16LE :: Word16 -> Builder
word16LE w = bytes (littleEndian 2 (fromIntegral w))

word32LE :: Word32 -> Builder
word32LE w = bytes (littleEndian 4 (fromIntegral w))

word64LE :: Word64 -> Builder
word64LE w = bytes (littleEndian 8 w)

word16BE :: Word16 -> Builder
word16BE w = bytes (reverse (littleEndian 2 (fromIntegral w)))

word32BE :: Word32 -> Builder
word32BE w = bytes (reverse (littleEndian 4 (fromIntegral w)))

word64BE :: Word64 -> Builder
word64BE w = bytes (reverse (littleEndian 8 w))

int16LE :: Int16 -> Builder
int16LE i = word16LE (fromIntegral i)

int32LE :: Int32 -> Builder
int32LE i = word32LE (fromIntegral i)

int64LE :: Int64 -> Builder
int64LE i = word64LE (fromIntegral i)

int16BE :: Int16 -> Builder
int16BE i = word16BE (fromIntegral i)

int32BE :: Int32 -> Builder
int32BE i = word32BE (fromIntegral i)

int64BE :: Int64 -> Builder
int64BE i = word64BE (fromIntegral i)
