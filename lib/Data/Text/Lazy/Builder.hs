module Data.Text.Lazy.Builder where

-- A text that is being put together from pieces: a function that puts the
-- pieces in front of what follows, so that adding a piece at either end
-- takes the same time whatever is there already.

import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T

newtype Builder = Builder (String -> String)

instance Semigroup Builder where
  (<>) (Builder a) (Builder b) = Builder (a . b)

instance Monoid Builder where
  mempty = Builder id

instance IsString Builder where
  fromString string = Builder (string ++)

fromText :: Text -> Builder
fromText text = Builder (T.unpack text ++)

fromLazyText :: Text -> Builder
fromLazyText = fromText

singleton :: Char -> Builder
singleton c = Builder (c :)

toLazyText :: Builder -> Text
toLazyText (Builder pieces) = T.pack (pieces "")
