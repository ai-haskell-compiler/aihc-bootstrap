module Data.Text where

-- A Text is bytes: its characters in UTF-8, as upstream. A part of a Text
-- shares the bytes of the whole, so taking a part is one step, and so is
-- asking for the length in bytes. AIHC's lexer depends on both.
--
-- What only looks at a text works on the bytes. What makes a new text
-- anyway (map, filter, reverse, ...) goes through the list of characters.
--
-- An offset is a number of bytes, and always where a character starts.

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (isSpace, ord)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.String (IsString (..))
import Prelude hiding (all, any, break, concat, concatMap, drop, dropWhile, elem, filter, foldr, head, init, last, length, lines, map, null, replicate, reverse, span, splitAt, tail, take, takeWhile, unlines, unwords, words, zip)
import qualified Prelude as P

newtype Text = Text ByteString

-- The bytes of two texts are in the order of their characters.
instance Eq Text where
  (==) (Text a) (Text b) = a == b

instance Ord Text where
  compare (Text a) (Text b) = compare a b

instance Show Text where
  showsPrec precedence text = showsPrec precedence (unpack text)

instance Semigroup Text where
  (<>) = append

instance Monoid Text where
  mempty = empty

instance IsString Text where
  fromString = pack

instance NFData Text

-- ---- The bytes ----

-- The character that starts at the offset.
foreign import prim "textCharAt" charAt :: ByteString -> Int -> Char

-- How many characters there are.
foreign import prim "textCount" countCharacters :: ByteString -> Int

-- The offset after that many characters, or the length if there are no
-- more than that.
foreign import prim "textOffset" offsetAfter :: ByteString -> Int -> Int

foreign import prim "bytesFromString" bytesFromString :: String -> ByteString

foreign import prim "bytesToString" bytesToString :: ByteString -> String

-- How many bytes the character takes.
width :: Char -> Int
width c
  | ord c < 0x80 = 1
  | ord c < 0x800 = 2
  | ord c < 0x10000 = 3
  | otherwise = 4

-- The bytes before the offset, and the bytes from it on.
before :: Int -> ByteString -> ByteString
before = B.take

from :: Int -> ByteString -> ByteString
from = B.drop

-- The offset of the first character from `offset` on that the test does
-- not hold for, or `end`.
scan :: (Char -> Bool) -> ByteString -> Int -> Int -> Int
scan test bytes end offset =
  if offset >= end
    then end
    else
      let c = charAt bytes offset
       in if test c then scan test bytes end (offset + width c) else offset

-- Where the character before the offset starts. The bytes of a character
-- after its first are 10xxxxxx.
previous :: ByteString -> Int -> Int
previous bytes offset =
  let byte = B.index bytes (offset - 1)
   in if byte >= 128 && byte < 192 then previous bytes (offset - 1) else offset - 1

-- The same as scan, from `offset` towards the start: the offset after the
-- last character before `offset` that the test does not hold for, or 0.
scanBack :: (Char -> Bool) -> ByteString -> Int -> Int
scanBack test bytes offset =
  if offset <= 0
    then 0
    else
      let start = previous bytes offset
       in if test (charAt bytes start) then scanBack test bytes start else offset

-- ---- To and from String ----

pack :: String -> Text
pack string = Text (bytesFromString string)

-- A short text becomes a list in one step. A long one becomes a list as
-- far as the list is looked at: it may be the rest of a file of which
-- only the start is wanted.
unpack :: Text -> String
unpack (Text bytes) =
  if B.length bytes <= 256 then bytesToString bytes else characters 0
  where
    end = B.length bytes
    characters offset =
      if offset >= end
        then []
        else
          let c = charAt bytes offset
           in c : characters (offset + width c)

empty :: Text
empty = Text B.empty

singleton :: Char -> Text
singleton c = pack [c]

-- ---- Basics ----

append :: Text -> Text -> Text
append (Text a) (Text b) = Text (B.append a b)

cons :: Char -> Text -> Text
cons c text = append (singleton c) text

snoc :: Text -> Char -> Text
snoc text c = append text (singleton c)

uncons :: Text -> Maybe (Char, Text)
uncons (Text bytes) =
  if B.null bytes
    then Nothing
    else
      let c = charAt bytes 0
       in Just (c, Text (from (width c) bytes))

unsnoc :: Text -> Maybe (Text, Char)
unsnoc (Text bytes) =
  if B.null bytes
    then Nothing
    else
      let start = previous bytes (B.length bytes)
       in Just (Text (before start bytes), charAt bytes start)

head :: Text -> Char
head text = case uncons text of
  Just (c, _) -> c
  Nothing -> error "Data.Text.head: empty input"

last :: Text -> Char
last text = case unsnoc text of
  Just (_, c) -> c
  Nothing -> error "Data.Text.last: empty input"

tail :: Text -> Text
tail text = case uncons text of
  Just (_, rest) -> rest
  Nothing -> error "Data.Text.tail: empty input"

init :: Text -> Text
init text = case unsnoc text of
  Just (start, _) -> start
  Nothing -> error "Data.Text.init: empty input"

null :: Text -> Bool
null (Text bytes) = B.null bytes

length :: Text -> Int
length (Text bytes) = countCharacters bytes

compareLength :: Text -> Int -> Ordering
compareLength text n = compare (length text) n

-- ---- Transformations ----

map :: (Char -> Char) -> Text -> Text
map f text = pack (P.map f (unpack text))

reverse :: Text -> Text
reverse text = pack (P.reverse (unpack text))

intercalate :: Text -> [Text] -> Text
intercalate (Text separator) texts = Text (B.intercalate separator (P.map (\(Text bytes) -> bytes) texts))

toLower :: Text -> Text
toLower = map Char.toLower

toUpper :: Text -> Text
toUpper = map Char.toUpper

-- Every occurrence of the first text replaced by the second.
replace :: Text -> Text -> Text -> Text
replace needle replacement haystack = intercalate replacement (splitOn needle haystack)

concat :: [Text] -> Text
concat texts = Text (B.concat (P.map (\(Text bytes) -> bytes) texts))

concatMap :: (Char -> Text) -> Text -> Text
concatMap f text = concat (P.map f (unpack text))

replicate :: Int -> Text -> Text
replicate n text = concat (P.replicate n text)

-- ---- Folds ----

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f start text = P.foldl' f start (unpack text)

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f end text = P.foldr f end (unpack text)

any :: (Char -> Bool) -> Text -> Bool
any test (Text bytes) = scan (not . test) bytes (B.length bytes) 0 < B.length bytes

all :: (Char -> Bool) -> Text -> Bool
all test (Text bytes) = scan test bytes (B.length bytes) 0 >= B.length bytes

elem :: Char -> Text -> Bool
elem c = any (== c)

-- ---- Parts ----

take :: Int -> Text -> Text
take n (Text bytes) = Text (before (offsetAfter bytes n) bytes)

drop :: Int -> Text -> Text
drop n (Text bytes) = Text (from (offsetAfter bytes n) bytes)

takeEnd :: Int -> Text -> Text
takeEnd n text = drop (length text - n) text

dropEnd :: Int -> Text -> Text
dropEnd n text = take (length text - n) text

splitAt :: Int -> Text -> (Text, Text)
splitAt n (Text bytes) = cut (offsetAfter bytes n) bytes

-- The text before the offset and the text from it on.
cut :: Int -> ByteString -> (Text, Text)
cut offset bytes = (Text (before offset bytes), Text (from offset bytes))

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile keep (Text bytes) = Text (before (scan keep bytes (B.length bytes) 0) bytes)

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile skip (Text bytes) = Text (from (scan skip bytes (B.length bytes) 0) bytes)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd keep (Text bytes) = Text (from (scanBack keep bytes (B.length bytes)) bytes)

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd skip (Text bytes) = Text (before (scanBack skip bytes (B.length bytes)) bytes)

dropAround :: (Char -> Bool) -> Text -> Text
dropAround skip text = dropWhileEnd skip (dropWhile skip text)

strip :: Text -> Text
strip = dropAround isSpace

stripStart :: Text -> Text
stripStart = dropWhile isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd isSpace

span :: (Char -> Bool) -> Text -> (Text, Text)
span keep (Text bytes) = cut (scan keep bytes (B.length bytes) 0) bytes

break :: (Char -> Bool) -> Text -> (Text, Text)
break stop = span (not . stop)

filter :: (Char -> Bool) -> Text -> Text
filter keep text = pack (P.filter keep (unpack text))

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex wanted (Text bytes) = search 0 0
  where
    end = B.length bytes
    search n offset =
      if offset >= end
        then Nothing
        else
          let c = charAt bytes offset
           in if wanted c then Just n else search (n + 1) (offset + width c)

find :: (Char -> Bool) -> Text -> Maybe Char
find wanted (Text bytes) =
  let offset = scan (not . wanted) bytes (B.length bytes) 0
   in if offset < B.length bytes then Just (charAt bytes offset) else Nothing

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition keep text = (filter keep text, filter (not . keep) text)

index :: Text -> Int -> Char
index (Text bytes) n =
  if n < 0 || offsetAfter bytes n >= B.length bytes
    then error "Data.Text.index: index out of bounds"
    else charAt bytes (offsetAfter bytes n)

count :: Text -> Text -> Int
count needle haystack = P.length (splitOn needle haystack) - 1

zip :: Text -> Text -> [(Char, Char)]
zip a b = P.zip (unpack a) (unpack b)

-- ---- Searching ----

-- A text that occurs in another one occurs where a character starts, so
-- these can look at the bytes.

isPrefixOf :: Text -> Text -> Bool
isPrefixOf (Text prefix) (Text bytes) = B.isPrefixOf prefix bytes

isSuffixOf :: Text -> Text -> Bool
isSuffixOf (Text suffix) (Text bytes) = B.isSuffixOf suffix bytes

isInfixOf :: Text -> Text -> Bool
isInfixOf (Text needle) (Text bytes) = B.bytesFind needle bytes >= 0

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix (Text prefix) (Text bytes) =
  if B.isPrefixOf prefix bytes then Just (Text (from (B.length prefix) bytes)) else Nothing

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix (Text suffix) (Text bytes) =
  if B.isSuffixOf suffix bytes then Just (Text (before (B.length bytes - B.length suffix) bytes)) else Nothing

-- The text before the first occurrence of the needle, and the text from the
-- needle on. If the needle does not occur, the whole text and nothing.
breakOn :: Text -> Text -> (Text, Text)
breakOn (Text needle) (Text bytes) =
  if B.null needle
    then error "Data.Text.breakOn: empty input"
    else case B.bytesFind needle bytes of
      -1 -> (Text bytes, empty)
      offset -> cut offset bytes

-- The same from the end: the text up to and including the last occurrence
-- of the needle, and the text after it. If the needle does not occur,
-- nothing and the whole text.
breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd (Text needle) (Text bytes) =
  if B.null needle
    then error "Data.Text.breakOnEnd: empty input"
    else cut (afterLast 0 0) bytes
  where
    -- `found` is the offset after the last occurrence that starts before
    -- `offset`.
    afterLast found offset = case B.bytesFind needle (from offset bytes) of
      -1 -> found
      next -> afterLast (offset + next + B.length needle) (offset + next + 1)

-- The pieces between the occurrences of the separator.
splitOn :: Text -> Text -> [Text]
splitOn (Text separator) (Text whole) =
  if B.null separator
    then error "Data.Text.splitOn: empty delimiter"
    else pieces whole
  where
    pieces bytes = case B.bytesFind separator bytes of
      -1 -> [Text bytes]
      offset -> Text (before offset bytes) : pieces (from (offset + B.length separator) bytes)

split :: (Char -> Bool) -> Text -> [Text]
split separates (Text whole) = pieces whole
  where
    pieces bytes =
      let end = B.length bytes
          offset = scan (not . separates) bytes end 0
       in if offset >= end
            then [Text bytes]
            else Text (before offset bytes) : pieces (from (offset + width (charAt bytes offset)) bytes)

-- ---- Lines and words ----

lines :: Text -> [Text]
lines text =
  if null text
    then []
    else case break (== '\n') text of
      (line, rest) -> line : lines (drop 1 rest)

unlines :: [Text] -> Text
unlines texts = concat (P.concatMap (\line -> [line, singleton '\n']) texts)

words :: Text -> [Text]
words text = P.filter (not . null) (split isSpace text)

unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')

-- ---- Patterns ----

pattern Empty :: Text
pattern Empty <- (null -> True)
  where
    Empty = empty

pattern (:<) :: Char -> Text -> Text
pattern c :< rest <- (uncons -> Just (c, rest))
  where
    (:<) c rest = cons c rest

pattern (:>) :: Text -> Char -> Text
pattern start :> c <- (unsnoc -> Just (start, c))
  where
    (:>) start c = snoc start c

infixr 5 :<

infixl 5 :>
