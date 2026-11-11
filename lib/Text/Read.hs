module Text.Read where

-- Reading values from the text that `show` gives. A derived Read is not
-- written yet; the instances here are for numbers, characters, strings,
-- lists and a few small types, which is what AIHC reads.

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isOctDigit, isSpace, ord, readLitChar)
import Data.Int
import Data.Word

type ReadS a = String -> [(a, String)]

class Read a where
  -- The values that the start of the string can be read as, each with
  -- what is left of the string. The Int is the precedence of the context,
  -- as for showsPrec.
  readsPrec :: Int -> ReadS a

  readList :: ReadS [a]
  readList = readBracketed

reads :: Read a => ReadS a
reads = readsPrec 0

readEither :: Read a => String -> Either String a
readEither string = case [a | (a, rest) <- reads string, all isSpace rest] of
  [a] -> Right a
  [] -> Left "Prelude.read: no parse"
  _ -> Left "Prelude.read: ambiguous parse"

readMaybe :: Read a => String -> Maybe a
readMaybe string = case readEither string of
  Right a -> Just a
  Left _ -> Nothing

read :: Read a => String -> a
read string = case readEither string of
  Right a -> a
  Left message -> error message

-- ---- Pieces ----

skipSpaces :: String -> String
skipSpaces = dropWhile isSpace

-- The rest of the string if it starts with the text, after any spaces.
expect :: String -> String -> [String]
expect text string =
  let rest = skipSpaces string
   in if take (length text) rest == text then [drop (length text) rest] else []

-- The reader, or the reader inside any number of parentheses.
optionalParens :: ReadS a -> ReadS a
optionalParens reader string =
  reader string
    ++ [(a, rest') | rest <- expect "(" string, (a, rest'') <- optionalParens reader rest, rest' <- expect ")" rest'']

-- [a, b, c]
readBracketed :: Read a => ReadS [a]
readBracketed string = [result | rest <- expect "[" string, result <- first rest]
  where
    first rest = [([], rest') | rest' <- expect "]" rest] ++ [(a : as, rest'') | (a, rest') <- reads rest, (as, rest'') <- more rest']
    more rest = [([], rest') | rest' <- expect "]" rest] ++ [(a : as, rest''') | rest' <- expect "," rest, (a, rest'') <- reads rest', (as, rest''') <- more rest'']

-- A whole number as Haskell writes it: decimal, 0x hexadecimal or 0o
-- octal, with an optional minus sign in front. A number with a fraction
-- or an exponent is not a whole number.
readInteger :: ReadS Integer
readInteger = optionalParens signed
  where
    signed string = case skipSpaces string of
      '-' : rest -> [(negate n, rest') | (n, rest') <- unsigned (skipSpaces rest)]
      rest -> unsigned rest
    unsigned string = case string of
      '0' : x : rest | (x == 'x' || x == 'X') && startsWith isHexDigit rest -> digits 16 isHexDigit rest
      '0' : o : rest | (o == 'o' || o == 'O') && startsWith isOctDigit rest -> digits 8 isOctDigit rest
      c : _ | isDigit c -> [result | result@(_, rest) <- digits 10 isDigit string, not (fractional rest)]
      _ -> []
    digits base isDigitOfBase string = case span isDigitOfBase string of
      (ds, rest) -> [(foldl (\n d -> n * base + toInteger (digitToInt d)) 0 ds, rest)]
    startsWith test string = case string of
      c : _ -> test c
      [] -> False
    fractional rest = case rest of
      '.' : d : _ -> isDigit d
      e : d : _ | e == 'e' || e == 'E' -> isDigit d || ((d == '-' || d == '+') && startsWith isDigit (drop 2 rest))
      _ -> False

readNumber :: Num a => ReadS a
readNumber string = [(fromInteger n, rest) | (n, rest) <- readInteger string]

-- ---- Instances ----

instance Read Integer where
  readsPrec _ = readInteger

instance Read Int where
  readsPrec _ = readNumber

instance Read Int8 where
  readsPrec _ = readNumber

instance Read Int16 where
  readsPrec _ = readNumber

instance Read Int32 where
  readsPrec _ = readNumber

instance Read Int64 where
  readsPrec _ = readNumber

instance Read Word where
  readsPrec _ = readNumber

instance Read Word8 where
  readsPrec _ = readNumber

instance Read Word16 where
  readsPrec _ = readNumber

instance Read Word32 where
  readsPrec _ = readNumber

instance Read Word64 where
  readsPrec _ = readNumber

-- 'c', and for a list of them "text".
instance Read Char where
  readsPrec _ = optionalParens (\string -> case skipSpaces string of
    '\'' : rest -> [(c, rest'') | c' : _ <- [rest], c' /= '\'', (c, '\'' : rest'') <- readLitChar rest]
    _ -> [])
  readList string = optionalParens quoted string ++ readBracketed string
    where
      quoted text = case skipSpaces text of
        '"' : rest -> characters rest
        _ -> []
      characters text = case text of
        '"' : rest -> [("", rest)]
        '\\' : '&' : rest -> characters rest
        '\\' : c : rest | isSpace c -> case dropWhile isSpace rest of
          '\\' : rest' -> characters rest'
          _ -> []
        [] -> []
        _ -> [(c : cs, rest') | (c, rest) <- readLitChar text, (cs, rest') <- characters rest]

instance Read a => Read [a] where
  readsPrec _ = readList

instance Read Bool where
  readsPrec _ = optionalParens (\string ->
    [(True, rest) | rest <- word "True" string] ++ [(False, rest) | rest <- word "False" string])

instance Read () where
  readsPrec _ = optionalParens (\string -> [((), rest') | rest <- expect "(" string, rest' <- expect ")" rest])

instance Read a => Read (Maybe a) where
  readsPrec precedence string =
    [(Nothing, rest) | rest <- word "Nothing" string]
      ++ [(Just a, rest') | precedence <= 10, rest <- word "Just" string, (a, rest') <- readsPrec 11 rest]
      -- Inside parentheses the context does not matter any more.
      ++ [(a, rest') | rest <- expect "(" string, (a, rest'') <- readsPrec 0 rest, rest' <- expect ")" rest'']

instance (Read a, Read b) => Read (a, b) where
  readsPrec _ string =
    [ ((a, b), rest5)
      | rest1 <- expect "(" string,
        (a, rest2) <- reads rest1,
        rest3 <- expect "," rest2,
        (b, rest4) <- reads rest3,
        rest5 <- expect ")" rest4
    ]

-- The rest of the string if it starts with the name, as a whole word.
word :: String -> String -> [String]
word name string = [rest | rest <- expect name string, not (continues rest)]
  where
    continues rest = case rest of
      c : _ -> isDigit c || c == '_' || c == '\'' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
      [] -> False
