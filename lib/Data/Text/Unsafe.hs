module Data.Text.Unsafe where

-- Text by UTF-8 byte offset, which is how the real Text is stored. Here a
-- Text is a list of characters, so an offset is found by adding up the
-- widths of the characters before it. An offset is always at the start of
-- a character.

import Data.Char (ord)
import Data.Text (Text (..))

-- A character and its width in bytes.
data Iter = Iter Char Int

-- How many bytes the character takes in UTF-8.
width :: Char -> Int
width c
  | ord c < 0x80 = 1
  | ord c < 0x800 = 2
  | ord c < 0x10000 = 3
  | otherwise = 4

-- How many characters the first `count` bytes of the string are.
charactersIn :: Int -> String -> Int
charactersIn count string = case string of
  c : rest | count > 0 -> 1 + charactersIn (count - width c) rest
  _ -> 0

lengthWord8 :: Text -> Int
lengthWord8 (Text string) = sum (map width string)

takeWord8 :: Int -> Text -> Text
takeWord8 count (Text string) = Text (take (charactersIn count string) string)

dropWord8 :: Int -> Text -> Text
dropWord8 count (Text string) = Text (drop (charactersIn count string) string)

-- The character at the byte offset.
iter :: Text -> Int -> Iter
iter text offset = case dropWord8 offset text of
  Text (c : _) -> Iter c (width c)
  Text [] -> error "Data.Text.Unsafe.iter: the offset is past the end"

unsafeHead :: Text -> Char
unsafeHead (Text string) = head string

unsafeTail :: Text -> Text
unsafeTail (Text string) = Text (tail string)
