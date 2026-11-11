module GHC.Fingerprint where

-- A fingerprint is the MD5 hash of some bytes, as two 64-bit words
-- (src/md5.c).

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word64, Word8)
import Foreign.Ptr (Ptr (..))

data Fingerprint = Fingerprint Word64 Word64
  deriving (Eq, Ord)

instance Show Fingerprint where
  show (Fingerprint a b) = hex a ++ hex b
    where
      hex w = let digits = showHexWord w in replicate (16 - length digits) '0' ++ digits
      showHexWord w = if w < 16 then [digit w] else showHexWord (w `div` 16) ++ [digit (w `mod` 16)]
      digit d = "0123456789abcdef" !! fromIntegral d

fingerprintBytes :: ByteString -> Fingerprint
fingerprintBytes bytes = case B.splitAt 8 (md5 bytes) of
  (first, second) -> Fingerprint (word first) (word second)
  where
    -- The highest byte first.
    word = B.foldl' (\w byte -> shiftL w 8 .|. fromIntegral byte) 0

-- The fingerprint of that many bytes from the pointer.
fingerprintData :: Ptr Word8 -> Int -> IO Fingerprint
fingerprintData (Ptr bytes) size = pure (fingerprintBytes (B.take size bytes))

fingerprintString :: String -> Fingerprint
fingerprintString string = fingerprintBytes (B.pack (concatMap utf32 string))
  where
    -- GHC hashes each character as four bytes, the highest first.
    utf32 c = let n = fromEnum c in map fromIntegral [n `div` 16777216, (n `div` 65536) `mod` 256, (n `div` 256) `mod` 256, n `mod` 256]

fingerprint0 :: Fingerprint
fingerprint0 = Fingerprint 0 0

foreign import prim "md5" md5 :: ByteString -> ByteString
