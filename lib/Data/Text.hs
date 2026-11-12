module Data.Text where

-- Text is a String with another name. AIHC converts between the two all the
-- time, and with a newtype pack and unpack cost nothing. Every function
-- here is the list function of the same meaning.

import Control.DeepSeq (NFData)
import Data.Char (isSpace)
import qualified Data.Char as Char
import qualified Data.List as List
import Data.String (IsString (..))
import Prelude hiding (all, any, break, concat, concatMap, drop, dropWhile, elem, filter, foldr, head, init, last, length, lines, map, null, replicate, reverse, span, splitAt, tail, take, takeWhile, unlines, unwords, words, zip)
import qualified Prelude as P

newtype Text = Text String

instance Eq Text where
  (==) (Text a) (Text b) = a == b

instance Ord Text where
  compare (Text a) (Text b) = compare a b

instance Show Text where
  showsPrec precedence (Text string) = showsPrec precedence string

instance Semigroup Text where
  (<>) (Text a) (Text b) = Text (a ++ b)

instance Monoid Text where
  mempty = empty

instance IsString Text where
  fromString = Text

instance NFData Text

-- ---- To and from String ----

pack :: String -> Text
pack = Text

unpack :: Text -> String
unpack (Text string) = string

empty :: Text
empty = Text ""

singleton :: Char -> Text
singleton c = Text [c]

-- ---- Basics ----

append :: Text -> Text -> Text
append (Text a) (Text b) = Text (a ++ b)

cons :: Char -> Text -> Text
cons c (Text string) = Text (c : string)

snoc :: Text -> Char -> Text
snoc (Text string) c = Text (string ++ [c])

uncons :: Text -> Maybe (Char, Text)
uncons (Text []) = Nothing
uncons (Text (c : rest)) = Just (c, Text rest)

unsnoc :: Text -> Maybe (Text, Char)
unsnoc (Text []) = Nothing
unsnoc (Text string) = Just (Text (P.init string), P.last string)

head :: Text -> Char
head (Text string) = P.head string

last :: Text -> Char
last (Text string) = P.last string

tail :: Text -> Text
tail (Text string) = Text (P.tail string)

init :: Text -> Text
init (Text string) = Text (P.init string)

null :: Text -> Bool
null (Text string) = P.null string

length :: Text -> Int
length (Text string) = P.length string

compareLength :: Text -> Int -> Ordering
compareLength text n = compare (length text) n

-- ---- Transformations ----

map :: (Char -> Char) -> Text -> Text
map f (Text string) = Text (P.map f string)

reverse :: Text -> Text
reverse (Text string) = Text (P.reverse string)

intercalate :: Text -> [Text] -> Text
intercalate (Text separator) texts = Text (List.intercalate separator (P.map unpack texts))

toLower :: Text -> Text
toLower = map Char.toLower

toUpper :: Text -> Text
toUpper = map Char.toUpper

-- Every occurrence of the first text replaced by the second.
replace :: Text -> Text -> Text -> Text
replace needle replacement haystack = intercalate replacement (splitOn needle haystack)

concat :: [Text] -> Text
concat texts = Text (P.concatMap unpack texts)

concatMap :: (Char -> Text) -> Text -> Text
concatMap f (Text string) = Text (P.concatMap (\c -> unpack (f c)) string)

replicate :: Int -> Text -> Text
replicate n (Text string) = Text (P.concat (P.replicate n string))

-- ---- Folds ----

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f start (Text string) = P.foldl' f start string

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f end (Text string) = P.foldr f end string

any :: (Char -> Bool) -> Text -> Bool
any test (Text string) = P.any test string

all :: (Char -> Bool) -> Text -> Bool
all test (Text string) = P.all test string

elem :: Char -> Text -> Bool
elem c (Text string) = P.elem c string

-- ---- Parts ----

take :: Int -> Text -> Text
take n (Text string) = Text (P.take n string)

drop :: Int -> Text -> Text
drop n (Text string) = Text (P.drop n string)

takeEnd :: Int -> Text -> Text
takeEnd n text = drop (length text - n) text

dropEnd :: Int -> Text -> Text
dropEnd n text = take (length text - n) text

splitAt :: Int -> Text -> (Text, Text)
splitAt n text = (take n text, drop n text)

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile keep (Text string) = Text (P.takeWhile keep string)

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile skip (Text string) = Text (P.dropWhile skip string)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd keep (Text string) = Text (P.reverse (P.takeWhile keep (P.reverse string)))

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd skip (Text string) = Text (List.dropWhileEnd skip string)

dropAround :: (Char -> Bool) -> Text -> Text
dropAround skip text = dropWhileEnd skip (dropWhile skip text)

strip :: Text -> Text
strip = dropAround isSpace

stripStart :: Text -> Text
stripStart = dropWhile isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd isSpace

span :: (Char -> Bool) -> Text -> (Text, Text)
span keep (Text string) = case P.span keep string of
  (front, back) -> (Text front, Text back)

break :: (Char -> Bool) -> Text -> (Text, Text)
break stop = span (not . stop)

filter :: (Char -> Bool) -> Text -> Text
filter keep (Text string) = Text (P.filter keep string)

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex wanted (Text string) = List.findIndex wanted string

find :: (Char -> Bool) -> Text -> Maybe Char
find wanted (Text string) = List.find wanted string

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition keep text = (filter keep text, filter (not . keep) text)

index :: Text -> Int -> Char
index (Text string) n = string !! n

count :: Text -> Text -> Int
count needle haystack = P.length (splitOn needle haystack) - 1

zip :: Text -> Text -> [(Char, Char)]
zip (Text a) (Text b) = P.zip a b

-- ---- Searching ----

isPrefixOf :: Text -> Text -> Bool
isPrefixOf (Text prefix) (Text string) = List.isPrefixOf prefix string

isSuffixOf :: Text -> Text -> Bool
isSuffixOf (Text suffix) (Text string) = List.isSuffixOf suffix string

isInfixOf :: Text -> Text -> Bool
isInfixOf (Text needle) (Text string) = List.isInfixOf needle string

stripPrefix :: Text -> Text -> Maybe Text
stripPrefix (Text prefix) (Text string) = fmap Text (List.stripPrefix prefix string)

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix (Text suffix) (Text string) =
  fmap (\rest -> Text (P.reverse rest)) (List.stripPrefix (P.reverse suffix) (P.reverse string))

-- The text before the first occurrence of the needle, and the text from the
-- needle on. If the needle does not occur, the whole text and nothing.
breakOn :: Text -> Text -> (Text, Text)
breakOn (Text needle) (Text string) =
  if P.null needle
    then error "Data.Text.breakOn: empty input"
    else case breakString needle string of
      (front, back) -> (Text front, Text back)

-- The same from the end: the text up to and including the last occurrence
-- of the needle, and the text after it. If the needle does not occur,
-- nothing and the whole text.
breakOnEnd :: Text -> Text -> (Text, Text)
breakOnEnd (Text needle) (Text string) =
  if P.null needle
    then error "Data.Text.breakOnEnd: empty input"
    else case breakString (P.reverse needle) (P.reverse string) of
      (back, front) -> (Text (P.reverse front), Text (P.reverse back))

breakString :: String -> String -> (String, String)
breakString _ [] = ([], [])
breakString needle string@(c : rest) =
  if List.isPrefixOf needle string
    then ([], string)
    else case breakString needle rest of
      (front, back) -> (c : front, back)

-- The pieces between the occurrences of the separator.
splitOn :: Text -> Text -> [Text]
splitOn (Text separator) (Text string) =
  if P.null separator
    then error "Data.Text.splitOn: empty delimiter"
    else P.map Text (pieces string)
  where
    pieces text = case breakString separator text of
      (piece, []) -> [piece]
      (piece, rest) -> piece : pieces (P.drop (P.length separator) rest)

split :: (Char -> Bool) -> Text -> [Text]
split separates (Text string) = P.map Text (pieces string)
  where
    pieces text = case P.break separates text of
      (piece, []) -> [piece]
      (piece, _ : rest) -> piece : pieces rest

-- ---- Lines and words ----

lines :: Text -> [Text]
lines (Text string) = P.map Text (P.lines string)

unlines :: [Text] -> Text
unlines texts = Text (P.unlines (P.map unpack texts))

words :: Text -> [Text]
words (Text string) = P.map Text (P.words string)

unwords :: [Text] -> Text
unwords texts = Text (P.unwords (P.map unpack texts))

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
