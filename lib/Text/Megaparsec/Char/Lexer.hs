-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Char.Lexer where

-- Parsers for the usual pieces of a language: white space and comments,
-- numbers, character literals.

import qualified Data.Char as Char
import Data.Functor (void)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C

-- White space as a language sees it: the three parsers are for space
-- characters (at least one), a line comment and a block comment.
space :: ParsecT e s m () -> ParsecT e s m () -> ParsecT e s m () -> ParsecT e s m ()
space sp line block = skipMany (choice [hidden sp, hidden line, hidden block])

-- The parser, and then the white space after it.
lexeme :: ParsecT e s m () -> ParsecT e s m a -> ParsecT e s m a
lexeme spc p = p <* spc

symbol :: Stream s => ParsecT e s m () -> Tokens s -> ParsecT e s m (Tokens s)
symbol spc text = lexeme spc (C.string text)

-- From the prefix to the end of the line, without the newline.
skipLineComment :: Stream s => Tokens s -> ParsecT e s m ()
skipLineComment prefix = C.string prefix *> void (takeWhileP (Just "character") (/= '\n'))

skipBlockComment :: Stream s => Tokens s -> Tokens s -> ParsecT e s m ()
skipBlockComment start end = C.string start >> void (manyTill anySingle (C.string end))

skipBlockCommentNested :: Stream s => Tokens s -> Tokens s -> ParsecT e s m ()
skipBlockCommentNested start end = C.string start >> void (manyTill inside (C.string end))
  where
    inside = skipBlockCommentNested start end <|> void anySingle

-- The number that the digits of the chunk are, in the base.
number :: (Stream s, Num a) => s -> a -> Tokens s -> a
number input base digits = foldl' step 0 (chunkToTokens (proxyOf input) digits)
  where
    step a c = a * base + fromIntegral (Char.digitToInt c)

decimal :: (Stream s, Num a) => ParsecT e s m a
decimal = (getInput >>= \input -> fmap (number input 10) (takeWhile1P (Just "digit") Char.isDigit)) <?> "integer"

binary :: (Stream s, Num a) => ParsecT e s m a
binary = (getInput >>= \input -> fmap (number input 2) (takeWhile1P Nothing (\c -> c == '0' || c == '1'))) <?> "binary integer"

octal :: (Stream s, Num a) => ParsecT e s m a
octal = (getInput >>= \input -> fmap (number input 8) (takeWhile1P Nothing Char.isOctDigit)) <?> "octal integer"

hexadecimal :: (Stream s, Num a) => ParsecT e s m a
hexadecimal = (getInput >>= \input -> fmap (number input 16) (takeWhile1P Nothing Char.isHexDigit)) <?> "hexadecimal integer"

-- An optional sign, white space, and the number.
signed :: (Stream s, Num a) => ParsecT e s m () -> ParsecT e s m a -> ParsecT e s m a
signed spc p = option id (lexeme spc sign) <*> p
  where
    sign = fmap (const id) (C.char '+') <|> fmap (const negate) (C.char '-')

-- A character as in a Haskell character literal, without the quotes.
charLiteral :: Stream s => ParsecT e s m Char
charLiteral = label "literal character" (lookAhead (count' 1 10 anySingle) >>= \r ->
  case listToMaybe (Char.readLitChar r) of
    Just (c, r') -> fmap (const c) (skipCount (length r - length r') anySingle)
    Nothing -> unexpected (Tokens (head r :| [])))

float :: ParsecT e s m a
float = interpreterTodo "Text.Megaparsec.Char.Lexer.float: floating point numbers"

foreign import prim "interpreterTodo" interpreterTodo :: String -> a
