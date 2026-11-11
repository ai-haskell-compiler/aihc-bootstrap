module Data.Aeson.Key where

import qualified Data.String as S
import Data.Text (Text)
import qualified Data.Text as T

-- The name of a field of a JSON object.
newtype Key = Key Text
  deriving (Eq, Ord)

instance Show Key where
  showsPrec precedence (Key text) = showsPrec precedence text

instance S.IsString Key where
  fromString = fromString

fromString :: String -> Key
fromString string = Key (T.pack string)

fromText :: Text -> Key
fromText = Key

toText :: Key -> Text
toText (Key text) = text

toString :: Key -> String
toString (Key text) = T.unpack text
