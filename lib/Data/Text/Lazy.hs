module Data.Text.Lazy (module Data.Text.Lazy, module Data.Text) where

-- Upstream's lazy text is a list of pieces. Here it is the same type as a
-- strict text.

import Data.Text

toStrict :: Text -> Text
toStrict text = text

fromStrict :: Text -> Text
fromStrict text = text
