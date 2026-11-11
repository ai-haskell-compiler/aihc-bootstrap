-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Class where

-- The parsers everything else is made of, as a class, so that they also
-- work in a monad transformer over a parser. AIHC's parsers of its own
-- file formats are ReaderT over a parser.
--
-- Upstream's class is `MonadParsec e s m`, where the monad `m` decides the
-- error type `e` and the stream `s`. Here the class has `m` only, and
-- says what `e` and `s` are with two associated types.

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Set (Set)
import Text.Megaparsec.Error
import Text.Megaparsec.Internal
import Text.Megaparsec.State
import Text.Megaparsec.Stream

type TokenOf m = Token (StreamOf m)

type TokensOf m = Tokens (StreamOf m)

type ErrorIn m = ParseError (StreamOf m) (ErrorOf m)

type StateOf m = State (StreamOf m) (ErrorOf m)

class MonadParsec m where
  type ErrorOf m
  type StreamOf m

  -- Fails with the error.
  parseError :: ErrorIn m -> m a

  -- Names what the parser stands for, for "expecting ..." in an error.
  label :: String -> m a -> m a

  -- If the parser fails, it is as if it had read nothing.
  try :: m a -> m a

  -- Runs the parser without reading anything, if it succeeds.
  lookAhead :: m a -> m a

  -- Succeeds if the parser fails. It never reads anything.
  notFollowedBy :: m a -> m ()

  -- If the parser fails, runs the function on the error, from the place
  -- of the failure. If that fails too, the first error stands.
  withRecovery :: (ErrorIn m -> m a) -> m a -> m a

  -- The error instead of a failure.
  observing :: m a -> m (Either (ErrorIn m) a)

  eof :: m ()

  -- Reads one token that the function accepts. The set is what to say
  -- was expected if it does not.
  token :: (TokenOf m -> Maybe a) -> Set (ErrorItem (TokenOf m)) -> m a

  -- Reads the chunk, compared with the function.
  tokens :: (TokensOf m -> TokensOf m -> Bool) -> TokensOf m -> m (TokensOf m)

  -- Reads tokens while they pass the test. The String names one such
  -- token.
  takeWhileP :: Maybe String -> (TokenOf m -> Bool) -> m (TokensOf m)

  -- As takeWhileP, but fails if there is not even one.
  takeWhile1P :: Maybe String -> (TokenOf m -> Bool) -> m (TokensOf m)

  -- Reads that many tokens.
  takeP :: Maybe String -> Int -> m (TokensOf m)

  getParserState :: m (StateOf m)

  updateParserState :: (StateOf m -> StateOf m) -> m ()

instance Stream s => MonadParsec (ParsecT e s m) where
  type ErrorOf (ParsecT e s m) = e
  type StreamOf (ParsecT e s m) = s
  parseError = pParseError
  label = pLabel
  try = pTry
  lookAhead = pLookAhead
  notFollowedBy = pNotFollowedBy
  withRecovery = pWithRecovery
  observing = pObserving
  eof = pEof
  token = pToken
  tokens = pTokens
  takeWhileP = pTakeWhileP
  takeWhile1P = pTakeWhile1P
  takeP = pTakeP
  getParserState = pGetParserState
  updateParserState = pUpdateParserState

instance MonadParsec m => MonadParsec (ReaderT r m) where
  type ErrorOf (ReaderT r m) = ErrorOf m
  type StreamOf (ReaderT r m) = StreamOf m
  parseError e = lift (parseError e)
  label name (ReaderT m) = ReaderT (\r -> label name (m r))
  try (ReaderT m) = ReaderT (\r -> try (m r))
  lookAhead (ReaderT m) = ReaderT (\r -> lookAhead (m r))
  notFollowedBy (ReaderT m) = ReaderT (\r -> notFollowedBy (m r))
  withRecovery recover (ReaderT m) = ReaderT (\r -> withRecovery (\e -> runReaderT (recover e) r) (m r))
  observing (ReaderT m) = ReaderT (\r -> observing (m r))
  eof = lift eof
  token test expected = lift (token test expected)
  tokens same chunk = lift (tokens same chunk)
  takeWhileP name test = lift (takeWhileP name test)
  takeWhile1P name test = lift (takeWhile1P name test)
  takeP name n = lift (takeP name n)
  getParserState = lift getParserState
  updateParserState f = lift (updateParserState f)

-- Leaves the parser out of "expecting ...".
hidden :: MonadParsec m => m a -> m a
hidden = label ""
