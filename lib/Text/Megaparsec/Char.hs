-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Char where

-- Parsers for streams of characters.

import Data.Char
import Data.Functor (void)
import Data.Proxy
import Text.Megaparsec

newline :: Stream s => ParsecT e s m (Token s)
newline = char '\n'

crlf :: Stream s => ParsecT e s m (Tokens s)
crlf = getInput >>= \input -> string (tokensToChunk (proxyOf input) "\r\n")

eol :: Stream s => ParsecT e s m (Tokens s)
eol = getInput >>= \input -> (fmap (tokenToChunk (proxyOf input)) newline <|> crlf) <?> "end of line"

tab :: Stream s => ParsecT e s m (Token s)
tab = char '\t'

-- Zero or more white space characters.
space :: Stream s => ParsecT e s m ()
space = void (takeWhileP (Just "white space") isSpace)

space1 :: Stream s => ParsecT e s m ()
space1 = void (takeWhile1P (Just "white space") isSpace)

-- As space, but not newlines.
hspace :: Stream s => ParsecT e s m ()
hspace = void (takeWhileP (Just "white space") isHSpace)

hspace1 :: Stream s => ParsecT e s m ()
hspace1 = void (takeWhile1P (Just "white space") isHSpace)

isHSpace :: Char -> Bool
isHSpace c = isSpace c && c /= '\n' && c /= '\r'

spaceChar :: Stream s => ParsecT e s m (Token s)
spaceChar = satisfy isSpace <?> "white space"

upperChar :: Stream s => ParsecT e s m (Token s)
upperChar = satisfy isUpper <?> "uppercase letter"

lowerChar :: Stream s => ParsecT e s m (Token s)
lowerChar = satisfy isLower <?> "lowercase letter"

letterChar :: Stream s => ParsecT e s m (Token s)
letterChar = satisfy isLetter <?> "letter"

alphaNumChar :: Stream s => ParsecT e s m (Token s)
alphaNumChar = satisfy isAlphaNum <?> "alphanumeric character"

printChar :: Stream s => ParsecT e s m (Token s)
printChar = satisfy isPrint <?> "printable character"

digitChar :: Stream s => ParsecT e s m (Token s)
digitChar = satisfy isDigit <?> "digit"

binDigitChar :: Stream s => ParsecT e s m (Token s)
binDigitChar = satisfy (\c -> c == '0' || c == '1') <?> "binary digit"

octDigitChar :: Stream s => ParsecT e s m (Token s)
octDigitChar = satisfy isOctDigit <?> "octal digit"

hexDigitChar :: Stream s => ParsecT e s m (Token s)
hexDigitChar = satisfy isHexDigit <?> "hexadecimal digit"

asciiChar :: Stream s => ParsecT e s m (Token s)
asciiChar = satisfy isAscii <?> "ASCII character"

char :: Stream s => Token s -> ParsecT e s m (Token s)
char = single

string :: Stream s => Tokens s -> ParsecT e s m (Tokens s)
string = chunk
