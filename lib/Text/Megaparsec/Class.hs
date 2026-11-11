-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Class where

-- Upstream, these are the methods of the class MonadParsec, so that they
-- also work in a monad transformer over a parser. AIHC only uses them at
-- the parser type itself, so here they are functions.

import Data.Set (Set)
import Text.Megaparsec.Error
import Text.Megaparsec.Internal
import Text.Megaparsec.State
import Text.Megaparsec.Stream

-- Fails with the error.
parseError :: ParseError s e -> ParsecT e s m a
parseError = pParseError

-- Names what the parser stands for, for "expecting ..." in an error.
label :: String -> ParsecT e s m a -> ParsecT e s m a
label = pLabel

-- Leaves the parser out of "expecting ...".
hidden :: ParsecT e s m a -> ParsecT e s m a
hidden = pLabel ""

-- If the parser fails, it is as if it had read nothing.
try :: ParsecT e s m a -> ParsecT e s m a
try = pTry

-- Runs the parser without reading anything, if it succeeds.
lookAhead :: ParsecT e s m a -> ParsecT e s m a
lookAhead = pLookAhead

-- Succeeds if the parser fails. It never reads anything.
notFollowedBy :: Stream s => ParsecT e s m a -> ParsecT e s m ()
notFollowedBy = pNotFollowedBy

-- If the parser fails, runs the function on the error, from the place of
-- the failure. If that fails too, the first error stands.
withRecovery :: (ParseError s e -> ParsecT e s m a) -> ParsecT e s m a -> ParsecT e s m a
withRecovery = pWithRecovery

-- The error instead of a failure.
observing :: ParsecT e s m a -> ParsecT e s m (Either (ParseError s e) a)
observing = pObserving

eof :: Stream s => ParsecT e s m ()
eof = pEof

-- Reads one token that the function accepts. The set is what to say was
-- expected if it does not.
token :: Stream s => (Token s -> Maybe a) -> Set (ErrorItem (Token s)) -> ParsecT e s m a
token = pToken

-- Reads the chunk, compared with the function.
tokens :: Stream s => (Tokens s -> Tokens s -> Bool) -> Tokens s -> ParsecT e s m (Tokens s)
tokens = pTokens

-- Reads tokens while they pass the test. The String names one such token.
takeWhileP :: Stream s => Maybe String -> (Token s -> Bool) -> ParsecT e s m (Tokens s)
takeWhileP = pTakeWhileP

-- As takeWhileP, but fails if there is not even one.
takeWhile1P :: Stream s => Maybe String -> (Token s -> Bool) -> ParsecT e s m (Tokens s)
takeWhile1P = pTakeWhile1P

-- Reads that many tokens.
takeP :: Stream s => Maybe String -> Int -> ParsecT e s m (Tokens s)
takeP = pTakeP

getParserState :: ParsecT e s m (State s e)
getParserState = pGetParserState

updateParserState :: (State s e -> State s e) -> ParsecT e s m ()
updateParserState = pUpdateParserState
