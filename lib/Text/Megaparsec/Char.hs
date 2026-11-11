-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Char where

-- Parsers for streams of characters.

import Data.Char
import Data.Functor (void)
import Data.Proxy
import Text.Megaparsec

newline :: MonadParsec m => m (TokenOf m)
newline = char '\n'

crlf :: MonadParsec m => m (TokensOf m)
crlf = getInput >>= \input -> string (tokensToChunk (proxyOf input) "\r\n")

eol :: MonadParsec m => m (TokensOf m)
eol = getInput >>= \input -> (fmap (tokenToChunk (proxyOf input)) newline <|> crlf) <?> "end of line"

tab :: MonadParsec m => m (TokenOf m)
tab = char '\t'

-- Zero or more white space characters.
space :: MonadParsec m => m ()
space = void (takeWhileP (Just "white space") isSpace)

space1 :: MonadParsec m => m ()
space1 = void (takeWhile1P (Just "white space") isSpace)

-- As space, but not newlines.
hspace :: MonadParsec m => m ()
hspace = void (takeWhileP (Just "white space") isHSpace)

hspace1 :: MonadParsec m => m ()
hspace1 = void (takeWhile1P (Just "white space") isHSpace)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n' && c /= '\r'

spaceChar :: MonadParsec m => m (TokenOf m)
spaceChar = satisfy isSpace <?> "white space"

upperChar :: MonadParsec m => m (TokenOf m)
upperChar = satisfy isUpper <?> "uppercase letter"

lowerChar :: MonadParsec m => m (TokenOf m)
lowerChar = satisfy isLower <?> "lowercase letter"

letterChar :: MonadParsec m => m (TokenOf m)
letterChar = satisfy isLetter <?> "letter"

alphaNumChar :: MonadParsec m => m (TokenOf m)
alphaNumChar = satisfy isAlphaNum <?> "alphanumeric character"

printChar :: MonadParsec m => m (TokenOf m)
printChar = satisfy isPrint <?> "printable character"

digitChar :: MonadParsec m => m (TokenOf m)
digitChar = satisfy isDigit <?> "digit"

binDigitChar :: MonadParsec m => m (TokenOf m)
binDigitChar = satisfy (\c -> c == '0' || c == '1') <?> "binary digit"

octDigitChar :: MonadParsec m => m (TokenOf m)
octDigitChar = satisfy isOctDigit <?> "octal digit"

hexDigitChar :: MonadParsec m => m (TokenOf m)
hexDigitChar = satisfy isHexDigit <?> "hexadecimal digit"

asciiChar :: MonadParsec m => m (TokenOf m)
asciiChar = satisfy isAscii <?> "ASCII character"

char :: MonadParsec m => TokenOf m -> m (TokenOf m)
char = single

string :: MonadParsec m => TokensOf m -> m (TokensOf m)
string = chunk
