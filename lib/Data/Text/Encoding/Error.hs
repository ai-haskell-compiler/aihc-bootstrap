module Data.Text.Encoding.Error (module Data.Text.Encoding) where

import Data.Text.Encoding (UnicodeException (..))

-- Replace what cannot be decoded by the replacement character.
lenientDecode :: String -> Maybe a -> Maybe Char
lenientDecode _ _ = Just '\65533'

strictDecode :: String -> Maybe a -> Maybe Char
strictDecode message _ = error message
