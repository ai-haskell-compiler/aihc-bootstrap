module Data.Text.Encoding where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Text (Text (..))

-- What decoding bytes that are not UTF-8 gives or throws.
data UnicodeException = DecodeError String

instance Show UnicodeException where
  show (DecodeError message) = message

instance Exception UnicodeException

encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text string) = bytesFromString string

decodeUtf8' :: ByteString -> Either UnicodeException Text
decodeUtf8' bytes =
  if bytesValidUtf8 bytes
    then Right (Text (bytesToString bytes))
    else Left (DecodeError "Cannot decode input: invalid UTF-8 stream")

decodeUtf8 :: ByteString -> Text
decodeUtf8 bytes = case decodeUtf8' bytes of
  Right text -> text
  Left problem -> error (show problem)

-- The handler is only needed for bytes that are not UTF-8, and what it
-- replaces is not worked out yet.
decodeUtf8With :: (String -> Maybe a -> Maybe Char) -> ByteString -> Text
decodeUtf8With _ bytes =
  if bytesValidUtf8 bytes
    then Text (bytesToString bytes)
    else interpreterTodo "Data.Text.Encoding.decodeUtf8With on bytes that are not UTF-8"

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With (\_ _ -> Just '\65533')

foreign import prim "bytesFromString" bytesFromString :: String -> ByteString

foreign import prim "bytesToString" bytesToString :: ByteString -> String

foreign import prim "bytesValidUtf8" bytesValidUtf8 :: ByteString -> Bool

foreign import prim "interpreterTodo" interpreterTodo :: String -> a
