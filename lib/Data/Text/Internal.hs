module Data.Text.Internal (Text, pattern Text) where

-- The constructor of the package text, as a pattern only: the array, the
-- offset and the length in bytes. See Data.Text.Array.

import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Text.Array (Array (..))
import Data.Text.Encoding (encodeUtf8)

pattern Text :: Array -> Int -> Int -> Text
pattern Text array offset count <- (internals -> (array, offset, count))

internals :: Text -> (Array, Int, Int)
internals text = (ByteArray (), 0, B.length (encodeUtf8 text))
