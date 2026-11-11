module Data.Aeson where

-- JSON: values, text to values and back, and the two classes that turn
-- values into Haskell data and back. It stands in for the package aeson,
-- with what AIHC uses of it. Text comes out as aeson writes it: no spaces,
-- the fields of an object sorted by name.

import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import Data.Char (chr, isDigit, isHexDigit, digitToInt, ord)
import Data.Map.Base (Map)
import qualified Data.Map.Base as Map
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Numeric (showHex)

infixr 8 .=

infixl 9 .:, .:?

type Object = KeyMap Value

type Array = Vector Value

-- A number is kept as an exact fraction.
data Value
  = Object Object
  | Array Array
  | String Text
  | Number Rational
  | Bool Bool
  | Null
  deriving (Eq, Show)

-- ---- Reading values out of JSON ----

-- A value, or what was wrong.
newtype Parser a = Parser (Either String a)

instance Functor Parser where
  fmap f (Parser result) = Parser (fmap f result)

instance Applicative Parser where
  pure a = Parser (Right a)
  (<*>) (Parser f) (Parser a) = Parser (f <*> a)

instance Monad Parser where
  (>>=) (Parser result) next = case result of
    Left message -> Parser (Left message)
    Right a -> next a

instance MonadFail Parser where
  fail message = Parser (Left message)

data Result a = Error String | Success a
  deriving (Eq, Show)

parseEither :: (a -> Parser b) -> a -> Either String b
parseEither parser a = case parser a of
  Parser result -> result

parseMaybe :: (a -> Parser b) -> a -> Maybe b
parseMaybe parser a = either (const Nothing) Just (parseEither parser a)

class FromJSON a where
  parseJSON :: Value -> Parser a

  -- So that a list of characters can be a JSON string.
  parseJSONList :: Value -> Parser [a]
  parseJSONList value = case value of
    Array values -> mapM parseJSON (V.toList values)
    _ -> typeMismatch "Array" value

fromJSON :: FromJSON a => Value -> Result a
fromJSON value = case parseEither parseJSON value of
  Left message -> Error message
  Right a -> Success a

typeMismatch :: String -> Value -> Parser a
typeMismatch expected value = fail ("expected " ++ expected ++ ", but encountered " ++ kind)
  where
    kind = case value of
      Object _ -> "Object"
      Array _ -> "Array"
      String _ -> "String"
      Number _ -> "Number"
      Bool _ -> "Boolean"
      Null -> "Null"

withObject :: String -> (Object -> Parser a) -> Value -> Parser a
withObject name parser value = case value of
  Object object' -> parser object'
  _ -> typeMismatch name value

withText :: String -> (Text -> Parser a) -> Value -> Parser a
withText name parser value = case value of
  String text -> parser text
  _ -> typeMismatch name value

withArray :: String -> (Array -> Parser a) -> Value -> Parser a
withArray name parser value = case value of
  Array values -> parser values
  _ -> typeMismatch name value

-- The field, which has to be there.
(.:) :: FromJSON a => Object -> Key -> Parser a
(.:) object' key = case KeyMap.lookup key object' of
  Just value -> parseJSON value
  Nothing -> fail ("key " ++ show (Key.toString key) ++ " not found")

-- The field, or Nothing if it is not there or is null.
(.:?) :: FromJSON a => Object -> Key -> Parser (Maybe a)
(.:?) object' key = case KeyMap.lookup key object' of
  Nothing -> pure Nothing
  Just Null -> pure Nothing
  Just value -> fmap Just (parseJSON value)

(.!=) :: Parser (Maybe a) -> a -> Parser a
(.!=) parser fallback = fmap (maybe fallback id) parser

instance FromJSON Value where
  parseJSON = pure

instance FromJSON Bool where
  parseJSON value = case value of
    Bool b -> pure b
    _ -> typeMismatch "Bool" value

instance FromJSON Text where
  parseJSON = withText "Text" pure

instance FromJSON Char where
  parseJSON value = case value of
    String text | T.length text == 1 -> pure (T.head text)
    _ -> typeMismatch "Char" value
  parseJSONList value = case value of
    String text -> pure (T.unpack text)
    _ -> typeMismatch "String" value

instance FromJSON a => FromJSON [a] where
  parseJSON = parseJSONList

instance FromJSON a => FromJSON (Maybe a) where
  parseJSON value = case value of
    Null -> pure Nothing
    _ -> fmap Just (parseJSON value)

-- A number without a fraction.
wholeNumber :: Num a => String -> Value -> Parser a
wholeNumber name value = case value of
  Number n | denominator n == 1 -> pure (fromInteger (numerator n))
  _ -> typeMismatch name value

instance FromJSON Int where
  parseJSON = wholeNumber "Int"

instance FromJSON Integer where
  parseJSON = wholeNumber "Integer"

instance FromJSON v => FromJSON (KeyMap v) where
  parseJSON = withObject "KeyMap" (\object' ->
    fmap KeyMap.fromList (mapM (\(key, value) -> fmap (\v -> (key, v)) (parseJSON value)) (KeyMap.toList object')))

instance FromJSON v => FromJSON (Map Text v) where
  parseJSON = withObject "Map" (\object' ->
    fmap Map.fromList (mapM (\(key, value) -> fmap (\v -> (Key.toText key, v)) (parseJSON value)) (KeyMap.toList object')))

-- ---- Making JSON values ----

class ToJSON a where
  toJSON :: a -> Value

  toJSONList :: [a] -> Value
  toJSONList list = Array (V.fromList (map toJSON list))

object :: [(Key, Value)] -> Value
object pairs = Object (KeyMap.fromList pairs)

(.=) :: ToJSON v => Key -> v -> (Key, Value)
(.=) key v = (key, toJSON v)

instance ToJSON Value where
  toJSON value = value

instance ToJSON Bool where
  toJSON = Bool

instance ToJSON Text where
  toJSON = String

instance ToJSON Char where
  toJSON c = String (T.singleton c)
  toJSONList string = String (T.pack string)

instance ToJSON a => ToJSON [a] where
  toJSON = toJSONList

instance ToJSON a => ToJSON (Maybe a) where
  toJSON = maybe Null toJSON

instance ToJSON Int where
  toJSON n = Number (toInteger n % 1)

instance ToJSON Integer where
  toJSON n = Number (n % 1)

instance ToJSON v => ToJSON (KeyMap v) where
  toJSON m = Object (fmap toJSON m)

instance ToJSON v => ToJSON (Map Text v) where
  toJSON m = Object (KeyMap.fromList [(Key.fromText key, toJSON v) | (key, v) <- Map.toList m])

-- ---- Values as text ----

-- Lazy byte strings are strict ones here (Data.ByteString.Lazy).
encode :: ToJSON a => a -> ByteString
encode a = TE.encodeUtf8 (T.pack (encodeValue (toJSON a) ""))

encodeValue :: Value -> ShowS
encodeValue value rest = case value of
  Null -> "null" ++ rest
  Bool b -> (if b then "true" else "false") ++ rest
  Number n -> encodeNumber n ++ rest
  String text -> encodeString (T.unpack text) rest
  Array values -> '[' : separated encodeValue (V.toList values) (']' : rest)
  Object fields -> '{' : separated encodeField (KeyMap.toList fields) ('}' : rest)
  where
    encodeField (key, v) end = encodeString (Key.toString key) (':' : encodeValue v end)

-- The items with commas between them.
separated :: (a -> ShowS) -> [a] -> ShowS
separated encodeItem items rest = case items of
  [] -> rest
  [item] -> encodeItem item rest
  item : more -> encodeItem item (',' : separated encodeItem more rest)

-- A whole number as its digits; anything else as the nearest Double is
-- shown.
encodeNumber :: Rational -> String
encodeNumber n =
  if denominator n == 1
    then show (numerator n)
    else show (fromRational n :: Double)

encodeString :: String -> ShowS
encodeString string rest = '"' : foldr escape ('"' : rest) string
  where
    escape c end = case c of
      '"' -> '\\' : '"' : end
      '\\' -> '\\' : '\\' : end
      '\n' -> '\\' : 'n' : end
      '\r' -> '\\' : 'r' : end
      '\t' -> '\\' : 't' : end
      _ ->
        if c < ' '
          then
            let digits = showHex (ord c) ""
             in '\\' : 'u' : replicate (4 - length digits) '0' ++ digits ++ end
          else c : end

-- ---- Text as values ----

eitherDecode :: FromJSON a => ByteString -> Either String a
eitherDecode bytes = case TE.decodeUtf8' bytes of
  Left _ -> Left "the input is not UTF-8"
  Right text -> case readValue (skipSpace (T.unpack text)) of
    Left message -> Left message
    Right (value, rest) ->
      if null (skipSpace rest)
        then parseEither parseJSON value
        else Left "there is something after the JSON value"

eitherDecodeStrict :: FromJSON a => ByteString -> Either String a
eitherDecodeStrict = eitherDecode

decode :: FromJSON a => ByteString -> Maybe a
decode bytes = either (const Nothing) Just (eitherDecode bytes)

decodeStrict :: FromJSON a => ByteString -> Maybe a
decodeStrict = decode

skipSpace :: String -> String
skipSpace = dropWhile (\c -> c == ' ' || c == '\n' || c == '\r' || c == '\t')

-- A value at the start of the string, and what follows it.
readValue :: String -> Either String (Value, String)
readValue string = case string of
  '{' : rest -> readFields [] (skipSpace rest)
  '[' : rest -> readItems [] (skipSpace rest)
  '"' : rest -> fmap (\(text, after) -> (String (T.pack text), after)) (readString rest)
  't' : 'r' : 'u' : 'e' : rest -> Right (Bool True, rest)
  'f' : 'a' : 'l' : 's' : 'e' : rest -> Right (Bool False, rest)
  'n' : 'u' : 'l' : 'l' : rest -> Right (Null, rest)
  c : _ | c == '-' || isDigit c -> readNumber string
  _ -> Left "this is not a JSON value"

readItems :: [Value] -> String -> Either String (Value, String)
readItems done string = case string of
  ']' : rest | null done -> Right (Array (V.fromList []), rest)
  _ -> readValue string >>= \(value, rest) -> case skipSpace rest of
    ',' : more -> readItems (value : done) (skipSpace more)
    ']' : more -> Right (Array (V.fromList (reverse (value : done))), more)
    _ -> Left "an array needs , or ] here"

readFields :: [(Key, Value)] -> String -> Either String (Value, String)
readFields done string = case string of
  '}' : rest | null done -> Right (Object KeyMap.empty, rest)
  '"' : rest -> readString rest >>= \(name, after) -> case skipSpace after of
    ':' : more -> readValue (skipSpace more) >>= \(value, rest') ->
      let fields = (Key.fromString name, value) : done
       in case skipSpace rest' of
            ',' : more' -> readFields fields (skipSpace more')
            '}' : more' -> Right (Object (KeyMap.fromList (reverse fields)), more')
            _ -> Left "an object needs , or } here"
    _ -> Left "an object needs : after the name of a field"
  _ -> Left "an object needs the name of a field here"

-- The characters up to the closing quote, with escapes replaced.
readString :: String -> Either String (String, String)
readString string = case string of
  '"' : rest -> Right ("", rest)
  '\\' : c : rest -> case c of
    'u' -> readUnicode rest >>= \(code, after) -> continue (chr code) after
    'n' -> continue '\n' rest
    't' -> continue '\t' rest
    'r' -> continue '\r' rest
    'b' -> continue '\b' rest
    'f' -> continue '\f' rest
    '"' -> continue '"' rest
    '\\' -> continue '\\' rest
    '/' -> continue '/' rest
    _ -> Left "a string has an escape that JSON does not know"
  c : rest -> continue c rest
  [] -> Left "a string does not end"
  where
    continue c rest = fmap (\(text, after) -> (c : text, after)) (readString rest)

-- Four hexadecimal digits, or two such groups that are one character
-- written as a pair of surrogates.
readUnicode :: String -> Either String (Int, String)
readUnicode string = case hex4 string of
  Nothing -> Left "a \\u escape needs four hexadecimal digits"
  Just (high, rest) ->
    if high >= 0xD800 && high < 0xDC00
      then case rest of
        '\\' : 'u' : more -> case hex4 more of
          Just (low, after) | low >= 0xDC00 && low < 0xE000 -> Right (0x10000 + (high - 0xD800) * 1024 + (low - 0xDC00), after)
          _ -> Right (0xFFFD, rest)
        _ -> Right (0xFFFD, rest)
      else Right (high, rest)
  where
    hex4 text = case splitAt 4 text of
      (digits, rest) ->
        if length digits == 4 && all isHexDigit digits
          then Just (foldl (\n d -> n * 16 + digitToInt d) 0 digits, rest)
          else Nothing

-- [-] digits [. digits] [e [+ or -] digits]
readNumber :: String -> Either String (Value, String)
readNumber string =
  let (sign, unsigned) = case string of
        '-' : rest -> (-1, rest)
        _ -> (1, string)
      (whole, afterWhole) = span isDigit unsigned
      (fraction, afterFraction) = case afterWhole of
        '.' : rest -> span isDigit rest
        _ -> ("", afterWhole)
      (power, afterPower) = case afterFraction of
        e : rest | e == 'e' || e == 'E' -> case rest of
          '-' : digits -> case span isDigit digits of
            (ds, after) -> (negate (number ds), after)
          '+' : digits -> case span isDigit digits of
            (ds, after) -> (number ds, after)
          _ -> case span isDigit rest of
            (ds, after) -> (number ds, after)
        _ -> (0, afterFraction)
      number digits = foldl (\n d -> n * 10 + toInteger (digitToInt d)) 0 digits
      mantissa = number (whole ++ fraction)
      scale = power - toInteger (length fraction)
      value = if scale >= 0 then (mantissa * 10 ^ scale) % 1 else mantissa % (10 ^ negate scale)
   in if null whole
        then Left "a number needs a digit"
        else Right (Number (sign * value), afterPower)
