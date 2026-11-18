-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec
  ( module Text.Megaparsec,
    module Text.Megaparsec.Class,
    module Text.Megaparsec.Error,
    module Text.Megaparsec.Pos,
    module Text.Megaparsec.State,
    module Text.Megaparsec.Stream,
    module Control.Monad.Combinators,
    ParsecT,
  )
where

-- A replacement for the package megaparsec, with the parts AIHC uses. The
-- parsers behave as upstream's do, error for error. What differs is listed
-- in Text.Megaparsec.Internal and Text.Megaparsec.Class.
--
-- getSourcePos needs the stream to be one whose lines can be counted
-- (TraversableStream); the constraint is not written, since the class has
-- only the monad to name.

import Control.Monad.Combinators
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as E
import Text.Megaparsec.Class
import Text.Megaparsec.Error
import Text.Megaparsec.Internal
import Text.Megaparsec.Pos
import Text.Megaparsec.State
import Text.Megaparsec.Stream

infix 0 <?>

type Parsec e s = ParsecT e s Identity

-- ---- Running a parser ----

-- The String names the input in error messages.
parse :: Parsec e s a -> String -> s -> Either (ParseErrorBundle s e) a
parse = runParser

parseMaybe :: Stream s => Parsec e s a -> s -> Maybe a
parseMaybe p s = case parse (p <* eof) "" s of
  Left _ -> Nothing
  Right x -> Just x

parseTest ::
  (ShowErrorComponent e, Show a, VisualStream s, TraversableStream s) =>
  Parsec e s a ->
  s ->
  IO ()
parseTest p input = case parse p "" input of
  Left e -> putStr (errorBundlePretty e)
  Right x -> print x

runParser :: Parsec e s a -> String -> s -> Either (ParseErrorBundle s e) a
runParser p name s = snd (runParser' p (initialState name s))

-- From the given state. It fails if the parser fails or registered errors.
runParser' :: Parsec e s a -> State s e -> (State s e, Either (ParseErrorBundle s e) a)
runParser' p s = case runParsecT p s of
  Reply s' _ result -> case result of
    OK _ x -> case stateParseErrors s' of
      [] -> (s', Right x)
      e : es -> (s', Left (toBundle (e :| es)))
    Error e -> (s', Left (toBundle (e :| stateParseErrors s')))
  where
    toBundle es = ParseErrorBundle (NE.sortWith errorOffset es) (statePosState s)

-- ---- Failing ----

-- Fails with what was found and what was expected.
failure :: MonadParsec m => Maybe (ErrorItem (TokenOf m)) -> Set (ErrorItem (TokenOf m)) -> m a
failure us ps = getOffset >>= \o -> parseError (TrivialError o us ps)

fancyFailure :: MonadParsec m => Set (ErrorFancy (ErrorOf m)) -> m a
fancyFailure xs = getOffset >>= \o -> parseError (FancyError o xs)

unexpected :: MonadParsec m => ErrorItem (TokenOf m) -> m a
unexpected item = failure (Just item) E.empty

customFailure :: MonadParsec m => ErrorOf m -> m a
customFailure e = fancyFailure (E.singleton (ErrorCustom e))

-- Changes every error of the parser, the registered ones too.
region :: MonadParsec m => (ErrorIn m -> ErrorIn m) -> m a -> m a
region f m = do
  state <- getParserState
  let deSoFar = stateParseErrors state
  updateParserState (\s -> s {stateParseErrors = []})
  r <- observing m
  updateParserState (\s -> s {stateParseErrors = map f (stateParseErrors s) ++ deSoFar})
  case r of
    Left err -> parseError (f err)
    Right x -> return x

-- Notes the error and goes on. The run fails at its end (runParser').
registerParseError :: MonadParsec m => ErrorIn m -> m ()
registerParseError e = updateParserState (\s -> s {stateParseErrors = e : stateParseErrors s})

registerFailure :: MonadParsec m => Maybe (ErrorItem (TokenOf m)) -> Set (ErrorItem (TokenOf m)) -> m ()
registerFailure us ps = getOffset >>= \o -> registerParseError (TrivialError o us ps)

registerFancyFailure :: MonadParsec m => Set (ErrorFancy (ErrorOf m)) -> m ()
registerFancyFailure xs = getOffset >>= \o -> registerParseError (FancyError o xs)

-- ---- Tokens ----

single :: MonadParsec m => TokenOf m -> m (TokenOf m)
single t = token (\x -> if x == t then Just x else Nothing) (E.singleton (Tokens (t :| [])))

satisfy :: MonadParsec m => (TokenOf m -> Bool) -> m (TokenOf m)
satisfy f = token (\x -> if f x then Just x else Nothing) E.empty

anySingle :: MonadParsec m => m (TokenOf m)
anySingle = satisfy (const True)

anySingleBut :: MonadParsec m => TokenOf m -> m (TokenOf m)
anySingleBut t = satisfy (/= t)

oneOf :: MonadParsec m => [TokenOf m] -> m (TokenOf m)
oneOf cs = satisfy (\x -> elem x cs)

noneOf :: MonadParsec m => [TokenOf m] -> m (TokenOf m)
noneOf cs = satisfy (\x -> notElem x cs)

chunk :: MonadParsec m => TokensOf m -> m (TokensOf m)
chunk = tokens (==)

(<?>) :: MonadParsec m => m a -> String -> m a
(<?>) p l = label l p

-- The tokens the parser read, with its result.
match :: MonadParsec m => m a -> m (TokensOf m, a)
match p = do
  o <- getOffset
  s <- getInput
  r <- p
  o' <- getOffset
  return (fst (fromJust (takeN_ (o' - o) s)), r)

takeRest :: MonadParsec m => m (TokensOf m)
takeRest = takeWhileP Nothing (const True)

atEnd :: MonadParsec m => m Bool
atEnd = option False (fmap (const True) (hidden eof))

-- ---- The state ----

getInput :: MonadParsec m => m (StreamOf m)
getInput = fmap stateInput getParserState

setInput :: MonadParsec m => StreamOf m -> m ()
setInput input = updateParserState (\s -> s {stateInput = input})

getOffset :: MonadParsec m => m Int
getOffset = fmap stateOffset getParserState

setOffset :: MonadParsec m => Int -> m ()
setOffset o = updateParserState (\s -> s {stateOffset = o})

setParserState :: MonadParsec m => StateOf m -> m ()
setParserState st = updateParserState (const st)

getSourcePos :: MonadParsec m => m SourcePos
getSourcePos = do
  st <- getParserState
  let pst = reachOffsetNoLine (stateOffset st) (statePosState st)
  setParserState st {statePosState = pst}
  return (pstateSourcePos pst)
