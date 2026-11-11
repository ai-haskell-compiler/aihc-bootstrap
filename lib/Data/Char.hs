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
