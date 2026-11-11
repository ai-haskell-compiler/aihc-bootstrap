module Data.ByteString.Lazy (module Data.ByteString) where

-- A lazy ByteString is a strict one here: nothing is read or built in
-- pieces. Lengths are Int64, as in the real module; the rest is
-- Data.ByteString.

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Prelude hiding (drop, length, take)

fromChunks :: [ByteString] -> ByteString
fromChunks = B.concat

toChunks :: ByteString -> [ByteString]
toChunks bytes = if B.null bytes then [] else [bytes]

length :: ByteString -> Int64
length bytes = fromIntegral (B.length bytes)

take :: Int64 -> ByteString -> ByteString
take n = B.take (fromIntegral n)

drop :: Int64 -> ByteString -> ByteString
drop n = B.drop (fromIntegral n)
