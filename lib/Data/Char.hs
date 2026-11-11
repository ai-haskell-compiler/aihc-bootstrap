module Data.Char where

-- What is known about a character comes from a table that was made with
-- GHC (src/unicode.c, scripts/gen-unicode.hs), so that every answer here
-- is GHC's. The definitions on top of the table are those of GHC's base.

data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned
  deriving (Eq, Ord, Show, Enum, Bounded)

-- 0: the general category, by position. 1, 2, 3: the character in upper,
-- lower and title case. 4, 5: 1 if it is upper case, lower case.
foreign import prim "charInfo" charInfo :: Int -> Char -> Int

generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum (charInfo 0 c)

ord :: Char -> Int
ord = numConvert

chr :: Int -> Char
chr = numConvert

isAscii :: Char -> Bool
isAscii c = c < '\128'

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\255'

isControl :: Char -> Bool
isControl c = generalCategory c == Control

isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isOctDigit :: Char -> Bool
isOctDigit c = c >= '0' && c <= '7'

isHexDigit :: Char -> Bool
isHexDigit c = isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

isAsciiUpper :: Char -> Bool
isAsciiUpper c = c >= 'A' && c <= 'Z'

isAsciiLower :: Char -> Bool
isAsciiLower c = c >= 'a' && c <= 'z'

isUpper :: Char -> Bool
isUpper c = charInfo 4 c == 1

isLower :: Char -> Bool
isLower c = charInfo 5 c == 1

isAlpha :: Char -> Bool
isAlpha c = generalCategory c <= OtherLetter

isLetter :: Char -> Bool
isLetter = isAlpha

isMark :: Char -> Bool
isMark c = let category = generalCategory c in category >= NonSpacingMark && category <= EnclosingMark

isNumber :: Char -> Bool
isNumber c = let category = generalCategory c in category >= DecimalNumber && category <= OtherNumber

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isNumber c

isPrint :: Char -> Bool
isPrint c = generalCategory c < LineSeparator

isPunctuation :: Char -> Bool
isPunctuation c = let category = generalCategory c in category >= ConnectorPunctuation && category <= OtherPunctuation

isSymbol :: Char -> Bool
isSymbol c = let category = generalCategory c in category >= MathSymbol && category <= OtherSymbol

isSeparator :: Char -> Bool
isSeparator c = let category = generalCategory c in category >= Space && category <= ParagraphSeparator

-- The space, the control characters that move the cursor, and every
-- character Unicode calls a space.
isSpace :: Char -> Bool
isSpace c
  | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v' || c == '\160' = True
  | c < '\887' = False
  | otherwise = generalCategory c == Space

toUpper :: Char -> Char
toUpper c = chr (charInfo 1 c)

toLower :: Char -> Char
toLower c = chr (charInfo 2 c)

toTitle :: Char -> Char
toTitle c = chr (charInfo 3 c)

digitToInt :: Char -> Int
digitToInt c
  | isDigit c = ord c - ord '0'
  | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = ord c - ord 'A' + 10
  | otherwise = error "Char.digitToInt: not a digit"

intToDigit :: Int -> Char
intToDigit n
  | n >= 0 && n <= 9 = chr (ord '0' + n)
  | n >= 10 && n <= 15 = chr (ord 'a' + n - 10)
  | otherwise = error "Char.intToDigit: not a digit"

-- Reads one character as it is written in a Haskell string literal: itself,
-- or an escape. The result is empty if the string does not start with one.
readLitChar :: String -> [(Char, String)]
readLitChar string = case string of
  [] -> []
  '\\' : rest -> readEscape rest
  c : rest -> [(c, rest)]

readEscape :: String -> [(Char, String)]
readEscape string = case string of
  'a' : rest -> [('\a', rest)]
  'b' : rest -> [('\b', rest)]
  'f' : rest -> [('\f', rest)]
  'n' : rest -> [('\n', rest)]
  'r' : rest -> [('\r', rest)]
  't' : rest -> [('\t', rest)]
  'v' : rest -> [('\v', rest)]
  '\\' : rest -> [('\\', rest)]
  '\'' : rest -> [('\'', rest)]
  '"' : rest -> [('"', rest)]
  '^' : c : rest | c >= '@' && c <= '_' -> [(chr (ord c - 64), rest)]
  'o' : rest -> readCode 8 isOctDigit rest
  'x' : rest -> readCode 16 isHexDigit rest
  c : _ | isDigit c -> readCode 10 isDigit string
  _ -> take 1 [(chr code, drop (length name) string) | (name, code) <- asciiNames, isPrefix name string]
  where
    isPrefix name text = take (length name) text == name

-- The number of a character, in the base.
readCode :: Int -> (Char -> Bool) -> String -> [(Char, String)]
readCode base isDigitOfBase string = case span isDigitOfBase string of
  ([], _) -> []
  (digits, rest) ->
    let code = foldl (\n c -> n * base + digitToInt c) 0 digits
     in if code > 0x10FFFF then [] else [(chr code, rest)]

-- The names of the control characters. SOH is before SO, so that the
-- longer name is tried first.
asciiNames :: [(String, Int)]
asciiNames =
  [ ("NUL", 0), ("SOH", 1), ("STX", 2), ("ETX", 3), ("EOT", 4), ("ENQ", 5), ("ACK", 6), ("BEL", 7),
    ("BS", 8), ("HT", 9), ("LF", 10), ("VT", 11), ("FF", 12), ("CR", 13), ("SO", 14), ("SI", 15),
    ("DLE", 16), ("DC1", 17), ("DC2", 18), ("DC3", 19), ("DC4", 20), ("NAK", 21), ("SYN", 22), ("ETB", 23),
    ("CAN", 24), ("EM", 25), ("SUB", 26), ("ESC", 27), ("FS", 28), ("GS", 29), ("RS", 30), ("US", 31),
    ("SP", 32), ("DEL", 127)
  ]
