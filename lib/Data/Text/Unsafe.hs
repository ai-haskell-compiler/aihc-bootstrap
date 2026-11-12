module Data.Text.Unsafe where

-- Text by byte offset. A Text is its characters in UTF-8, and an offset
-- is always where a character starts.

import qualified Data.ByteString as B
import Data.Text (Text (..), charAt, width)

-- A character and its width in bytes.
data Iter = Iter Char Int

lengthWord8 :: Text -> Int
lengthWord8 (Text bytes) = B.length bytes

takeWord8 :: Int -> Text -> Text
takeWord8 count (Text bytes) = Text (B.take count bytes)

dropWord8 :: Int -> Text -> Text
dropWord8 count (Text bytes) = Text (B.drop count bytes)

-- The character at the byte offset.
iter :: Text -> Int -> Iter
iter (Text bytes) offset = let c = charAt bytes offset in Iter c (width c)

unsafeHead :: Text -> Char
unsafeHead (Text bytes) = charAt bytes 0

unsafeTail :: Text -> Text
unsafeTail (Text bytes) = Text (B.drop (width (charAt bytes 0)) bytes)
