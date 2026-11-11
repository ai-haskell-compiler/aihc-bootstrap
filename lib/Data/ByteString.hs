module Data.ByteString where

-- A ByteString is an array of bytes in the interpreter (src/bytes.c). The
-- functions that need the array are builtins; the others are written with
-- them. A part of a ByteString shares the bytes of the whole.

import Control.DeepSeq (NFData)
import Control.Exception (IOException (..), throwIO)
import Data.String (IsString (..))
import Data.Word (Word8)
import Prelude hiding (break, concat, drop, dropWhile, filter, head, init, last, length, map, null, readFile, replicate, reverse, span, splitAt, tail, take, takeWhile, writeFile, elem, all, any)
import qualified Prelude as P
import System.IO (Handle (..))

data ByteString

instance Eq ByteString where
  (==) a b = bytesCompare a b == 0

instance Ord ByteString where
  compare a b = compare (bytesCompare a b) 0

instance Show ByteString where
  showsPrec precedence bytes = showsPrec precedence (P.map (toEnum . fromIntegral) (unpack bytes) :: String)

instance Semigroup ByteString where
  (<>) = append

instance Monoid ByteString where
  mempty = empty

-- A string literal gives the low eight bits of each character.
instance IsString ByteString where
  fromString string = pack (P.map (fromIntegral . fromEnum) string)

instance NFData ByteString

-- ---- The builtins ----

foreign import prim "bytesPack" pack :: [Word8] -> ByteString

foreign import prim "bytesUnpack" unpack :: ByteString -> [Word8]

foreign import prim "bytesLength" length :: ByteString -> Int

foreign import prim "bytesIndex" index :: ByteString -> Int -> Word8

foreign import prim "bytesConcat" concat :: [ByteString] -> ByteString

-- So many bytes from an offset, as far as they exist.
foreign import prim "bytesSlice" slice :: Int -> Int -> ByteString -> ByteString

-- Negative, zero or positive.
foreign import prim "bytesCompare" bytesCompare :: ByteString -> ByteString -> Int

-- Where the first occurs in the second, or -1.
foreign import prim "bytesFind" bytesFind :: ByteString -> ByteString -> Int

foreign import prim "bytesReadFile" bytesReadFile :: String -> IO (Maybe ByteString)

foreign import prim "bytesWriteFile" bytesWriteFile :: String -> ByteString -> IO Bool

foreign import prim "bytesHPut" bytesHPut :: Int -> ByteString -> IO ()

-- ---- Basics ----

empty :: ByteString
empty = pack []

singleton :: Word8 -> ByteString
singleton byte = pack [byte]

null :: ByteString -> Bool
null bytes = length bytes == 0

append :: ByteString -> ByteString -> ByteString
append a b = concat [a, b]

cons :: Word8 -> ByteString -> ByteString
cons byte bytes = concat [singleton byte, bytes]

snoc :: ByteString -> Word8 -> ByteString
snoc bytes byte = concat [bytes, singleton byte]

head :: ByteString -> Word8
head bytes = index bytes 0

last :: ByteString -> Word8
last bytes = index bytes (length bytes - 1)

tail :: ByteString -> ByteString
tail = drop 1

init :: ByteString -> ByteString
init bytes = take (length bytes - 1) bytes

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons bytes = if null bytes then Nothing else Just (head bytes, tail bytes)

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc bytes = if null bytes then Nothing else Just (init bytes, last bytes)

replicate :: Int -> Word8 -> ByteString
replicate n byte = pack (P.replicate n byte)

copy :: ByteString -> ByteString
copy bytes = bytes

fromStrict :: ByteString -> ByteString
fromStrict bytes = bytes

toStrict :: ByteString -> ByteString
toStrict bytes = bytes

-- ---- Parts ----

take :: Int -> ByteString -> ByteString
take n = slice 0 n

drop :: Int -> ByteString -> ByteString
drop n bytes = slice n (length bytes) bytes

splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n bytes = (take n bytes, drop n bytes)

takeEnd :: Int -> ByteString -> ByteString
takeEnd n bytes = drop (length bytes - n) bytes

dropEnd :: Int -> ByteString -> ByteString
dropEnd n bytes = take (length bytes - n) bytes

-- How many bytes at the start satisfy the test.
countWhile :: (Word8 -> Bool) -> ByteString -> Int
countWhile test bytes = go 0
  where
    go n = if n < length bytes && test (index bytes n) then go (n + 1) else n

-- How many bytes at the end satisfy the test.
countWhileEnd :: (Word8 -> Bool) -> ByteString -> Int
countWhileEnd test bytes = go 0
  where
    go n = if n < length bytes && test (index bytes (length bytes - 1 - n)) then go (n + 1) else n

takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile keep bytes = take (countWhile keep bytes) bytes

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile skip bytes = drop (countWhile skip bytes) bytes

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd keep bytes = takeEnd (countWhileEnd keep bytes) bytes

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd skip bytes = dropEnd (countWhileEnd skip bytes) bytes

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span keep bytes = splitAt (countWhile keep bytes) bytes

break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break stop = span (not . stop)

-- The pieces between the bytes that the test accepts. Nothing gives no
-- pieces.
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith separates bytes = if null bytes then [] else pieces bytes
  where
    pieces rest = case break separates rest of
      (piece, more) -> if null more then [piece] else piece : pieces (drop 1 more)

split :: Word8 -> ByteString -> [ByteString]
split byte = splitWith (== byte)

-- ---- Whole-string functions ----

map :: (Word8 -> Word8) -> ByteString -> ByteString
map f bytes = pack (P.map f (unpack bytes))

reverse :: ByteString -> ByteString
reverse bytes = pack (P.reverse (unpack bytes))

filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter keep bytes = pack (P.filter keep (unpack bytes))

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f start bytes = P.foldl' f start (unpack bytes)

foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr f end bytes = P.foldr f end (unpack bytes)

all :: (Word8 -> Bool) -> ByteString -> Bool
all test bytes = countWhile test bytes == length bytes

any :: (Word8 -> Bool) -> ByteString -> Bool
any test bytes = not (all (not . test) bytes)

elem :: Word8 -> ByteString -> Bool
elem byte = any (== byte)

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate _ [] = empty
intercalate separator (first : rest) = concat (first : P.concatMap (\piece -> [separator, piece]) rest)

concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f bytes = concat (P.map f (unpack bytes))

-- ---- Searching ----

elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex byte = findIndex (== byte)

findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex wanted bytes =
  let n = countWhile (not . wanted) bytes
   in if n == length bytes then Nothing else Just n

elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd byte bytes =
  let n = countWhileEnd (/= byte) bytes
   in if n == length bytes then Nothing else Just (length bytes - 1 - n)

isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf prefix bytes = take (length prefix) bytes == prefix

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf suffix bytes = takeEnd (length suffix) bytes == suffix && length suffix <= length bytes

isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf needle bytes = bytesFind needle bytes >= 0

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix prefix bytes = if isPrefixOf prefix bytes then Just (drop (length prefix) bytes) else Nothing

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix suffix bytes = if isSuffixOf suffix bytes then Just (dropEnd (length suffix) bytes) else Nothing

-- The bytes before the first occurrence of the needle, and the bytes from
-- the needle on. If it does not occur, everything and nothing.
breakSubstring :: ByteString -> ByteString -> (ByteString, ByteString)
breakSubstring needle bytes =
  let at = bytesFind needle bytes
   in if at < 0 then (bytes, empty) else splitAt at bytes

-- ---- Files ----

readFile :: FilePath -> IO ByteString
readFile path = bytesReadFile path >>= \found -> case found of
  Just bytes -> pure bytes
  Nothing -> throwIO (IOError (path ++ ": cannot read the file"))

writeFile :: FilePath -> ByteString -> IO ()
writeFile path bytes = bytesWriteFile path bytes >>= \ok ->
  if ok then pure () else throwIO (IOError (path ++ ": cannot write the file"))

hPut :: Handle -> ByteString -> IO ()
hPut (Handle descriptor) bytes = bytesHPut descriptor bytes

putStr :: ByteString -> IO ()
putStr = bytesHPut 1

-- ---- Handles ----

hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

-- Everything that is left to read.
hGetContents :: Handle -> IO ByteString
hGetContents (Handle descriptor) = fmap (maybe empty id) (handleRead descriptor False)

-- The next line, without its newline.
hGetLine :: Handle -> IO ByteString
hGetLine (Handle descriptor) = handleRead descriptor True >>= \found -> case found of
  Just bytes -> pure bytes
  Nothing -> throwIO (IOError "end of file")

getContents :: IO ByteString
getContents = hGetContents (Handle 0)

getLine :: IO ByteString
getLine = hGetLine (Handle 0)

foreign import prim "handleRead" handleRead :: Int -> Bool -> IO (Maybe ByteString)
