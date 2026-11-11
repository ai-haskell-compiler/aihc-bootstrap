module Data.Char where

-- Classification is written for ASCII. A question about any other
-- character stops the interpreter instead of giving an answer that might
-- be wrong; Unicode tables are added when that happens.

ord :: Char -> Int
ord = numConvert

chr :: Int -> Char
chr = numConvert

isAscii :: Char -> Bool
isAscii c = c < '\128'

isLatin1 :: Char -> Bool
isLatin1 c = c <= '\255'

isControl :: Char -> Bool
isControl c = if isAscii c then c < ' ' || c == '\DEL' else beyondAscii "isControl"

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
isUpper c = if isAscii c then isAsciiUpper c else beyondAscii "isUpper"

isLower :: Char -> Bool
isLower c = if isAscii c then isAsciiLower c else beyondAscii "isLower"

isAlpha :: Char -> Bool
isAlpha c = if isAscii c then isAsciiUpper c || isAsciiLower c else beyondAscii "isAlpha"

isLetter :: Char -> Bool
isLetter = isAlpha

isAlphaNum :: Char -> Bool
isAlphaNum c = if isAscii c then isAsciiUpper c || isAsciiLower c || isDigit c else beyondAscii "isAlphaNum"

isPrint :: Char -> Bool
isPrint c = if isAscii c then c >= ' ' && c < '\DEL' else beyondAscii "isPrint"

isPunctuation :: Char -> Bool
isPunctuation c =
  if isAscii c
    then elem c "!\"#%&'()*,-./:;?@[\\]_{}"
    else beyondAscii "isPunctuation"

isSymbol :: Char -> Bool
isSymbol c = if isAscii c then elem c "$+<=>^`|~" else beyondAscii "isSymbol"

-- The white space of ASCII and the no-break space. The other spaces of
-- Unicode are not known here.
isSpace :: Char -> Bool
isSpace c
  | c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == '\v' = True
  | isAscii c = False
  | c == '\160' = True
  | otherwise = beyondAscii "isSpace"

toUpper :: Char -> Char
toUpper c
  | isAsciiLower c = chr (ord c - 32)
  | isAscii c = c
  | otherwise = beyondAscii "toUpper"

toLower :: Char -> Char
toLower c
  | isAsciiUpper c = chr (ord c + 32)
  | isAscii c = c
  | otherwise = beyondAscii "toLower"

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

beyondAscii :: String -> a
beyondAscii name = interpreterTodo ("Data.Char." ++ name ++ " of a character beyond ASCII")

foreign import prim "interpreterTodo" interpreterTodo :: String -> a

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
