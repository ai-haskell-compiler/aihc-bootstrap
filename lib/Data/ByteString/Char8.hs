module Data.ByteString.Char8 (module Data.ByteString) where

-- The same ByteString, with each byte seen as a character. The functions of
-- Data.ByteString that do not mention a byte are used as they are.

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Char (isDigit, isSpace)
import Data.Word (Word8)
import Prelude hiding (all, any, break, concatMap, dropWhile, elem, filter, head, last, lines, map, putStrLn, replicate, span, takeWhile, unlines, unwords, words)
import qualified Prelude as P

toChar :: Word8 -> Char
toChar byte = toEnum (fromIntegral byte)

-- The low eight bits of the character.
toByte :: Char -> Word8
toByte c = fromIntegral (fromEnum c)

pack :: String -> ByteString
pack string = B.pack (P.map toByte string)

unpack :: ByteString -> String
unpack bytes = P.map toChar (B.unpack bytes)

singleton :: Char -> ByteString
singleton c = B.singleton (toByte c)

cons :: Char -> ByteString -> ByteString
cons c = B.cons (toByte c)

snoc :: ByteString -> Char -> ByteString
snoc bytes c = B.snoc bytes (toByte c)

head :: ByteString -> Char
head bytes = toChar (B.head bytes)

last :: ByteString -> Char
last bytes = toChar (B.last bytes)

index :: ByteString -> Int -> Char
index bytes n = toChar (B.index bytes n)

uncons :: ByteString -> Maybe (Char, ByteString)
uncons bytes = fmap (\(byte, rest) -> (toChar byte, rest)) (B.uncons bytes)

unsnoc :: ByteString -> Maybe (ByteString, Char)
unsnoc bytes = fmap (\(rest, byte) -> (rest, toChar byte)) (B.unsnoc bytes)

replicate :: Int -> Char -> ByteString
replicate n c = B.replicate n (toByte c)

map :: (Char -> Char) -> ByteString -> ByteString
map f = B.map (toByte . f . toChar)

filter :: (Char -> Bool) -> ByteString -> ByteString
filter keep = B.filter (keep . toChar)

all :: (Char -> Bool) -> ByteString -> Bool
all test = B.all (test . toChar)

any :: (Char -> Bool) -> ByteString -> Bool
any test = B.any (test . toChar)

elem :: Char -> ByteString -> Bool
elem c = B.elem (toByte c)

concatMap :: (Char -> ByteString) -> ByteString -> ByteString
concatMap f = B.concatMap (f . toChar)

foldl' :: (a -> Char -> a) -> a -> ByteString -> a
foldl' f = B.foldl' (\total byte -> f total (toChar byte))

takeWhile :: (Char -> Bool) -> ByteString -> ByteString
takeWhile keep = B.takeWhile (keep . toChar)

dropWhile :: (Char -> Bool) -> ByteString -> ByteString
dropWhile skip = B.dropWhile (skip . toChar)

takeWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
takeWhileEnd keep = B.takeWhileEnd (keep . toChar)

dropWhileEnd :: (Char -> Bool) -> ByteString -> ByteString
dropWhileEnd skip = B.dropWhileEnd (skip . toChar)

span :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
span keep = B.span (keep . toChar)

break :: (Char -> Bool) -> ByteString -> (ByteString, ByteString)
break stop = B.break (stop . toChar)

strip :: ByteString -> ByteString
strip bytes = dropWhileEnd isSpace (dropWhile isSpace bytes)

split :: Char -> ByteString -> [ByteString]
split c = B.split (toByte c)

splitWith :: (Char -> Bool) -> ByteString -> [ByteString]
splitWith separates = B.splitWith (separates . toChar)

elemIndex :: Char -> ByteString -> Maybe Int
elemIndex c = B.elemIndex (toByte c)

elemIndexEnd :: Char -> ByteString -> Maybe Int
elemIndexEnd c = B.elemIndexEnd (toByte c)

findIndex :: (Char -> Bool) -> ByteString -> Maybe Int
findIndex wanted = B.findIndex (wanted . toChar)

-- The lines, without their newlines. A newline at the very end does not
-- start another line.
lines :: ByteString -> [ByteString]
lines bytes =
  if B.null bytes
    then []
    else case break (== '\n') bytes of
      (line, rest) -> line : lines (B.drop 1 rest)

unlines :: [ByteString] -> ByteString
unlines list = B.concat (P.concatMap (\line -> [line, singleton '\n']) list)

words :: ByteString -> [ByteString]
words bytes = P.filter (not . B.null) (splitWith isSpace bytes)

unwords :: [ByteString] -> ByteString
unwords = B.intercalate (singleton ' ')

-- A number at the start, with an optional sign, and what follows it.
readInt :: ByteString -> Maybe (Int, ByteString)
readInt bytes = case uncons bytes of
  Just ('-', rest) -> fmap (\(n, more) -> (negate n, more)) (readDigits rest)
  Just ('+', rest) -> readDigits rest
  _ -> readDigits bytes

readInteger :: ByteString -> Maybe (Integer, ByteString)
readInteger bytes = fmap (\(n, rest) -> (fromIntegral n, rest)) (readInt bytes)

readDigits :: ByteString -> Maybe (Int, ByteString)
readDigits bytes = case span isDigit bytes of
  (digits, rest) ->
    if B.null digits
      then Nothing
      else Just (foldl' (\n c -> n * 10 + (fromEnum c - fromEnum '0')) 0 digits, rest)

putStrLn :: ByteString -> IO ()
putStrLn bytes = B.putStr bytes >> B.putStr (singleton '\n')
