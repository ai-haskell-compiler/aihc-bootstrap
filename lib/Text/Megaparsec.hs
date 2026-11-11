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
failure :: Maybe (ErrorItem (Token s)) -> Set (ErrorItem (Token s)) -> ParsecT e s m a
failure us ps = getOffset >>= \o -> parseError (TrivialError o us ps)

fancyFailure :: Set (ErrorFancy e) -> ParsecT e s m a
fancyFailure xs = getOffset >>= \o -> parseError (FancyError o xs)

unexpected :: ErrorItem (Token s) -> ParsecT e s m a
unexpected item = failure (Just item) E.empty

customFailure :: e -> ParsecT e s m a
customFailure e = fancyFailure (E.singleton (ErrorCustom e))

-- Changes every error of the parser, the registered ones too.
region :: (ParseError s e -> ParseError s e) -> ParsecT e s m a -> ParsecT e s m a
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
registerParseError :: ParseError s e -> ParsecT e s m ()
registerParseError e = updateParserState (\s -> s {stateParseErrors = e : stateParseErrors s})

registerFailure :: Maybe (ErrorItem (Token s)) -> Set (ErrorItem (Token s)) -> ParsecT e s m ()
registerFailure us ps = getOffset >>= \o -> registerParseError (TrivialError o us ps)

registerFancyFailure :: Set (ErrorFancy e) -> ParsecT e s m ()
registerFancyFailure xs = getOffset >>= \o -> registerParseError (FancyError o xs)

-- ---- Tokens ----

single :: Stream s => Token s -> ParsecT e s m (Token s)
single t = token (\x -> if x == t then Just x else Nothing) (E.singleton (Tokens (t :| [])))

satisfy :: Stream s => (Token s -> Bool) -> ParsecT e s m (Token s)
satisfy f = token (\x -> if f x then Just x else Nothing) E.empty

anySingle :: Stream s => ParsecT e s m (Token s)
anySingle = satisfy (const True)

anySingleBut :: Stream s => Token s -> ParsecT e s m (Token s)
anySingleBut t = satisfy (/= t)

oneOf :: Stream s => [Token s] -> ParsecT e s m (Token s)
oneOf cs = satisfy (\x -> elem x cs)

noneOf :: Stream s => [Token s] -> ParsecT e s m (Token s)
noneOf cs = satisfy (\x -> notElem x cs)

chunk :: Stream s => Tokens s -> ParsecT e s m (Tokens s)
chunk = tokens (==)

(<?>) :: ParsecT e s m a -> String -> ParsecT e s m a
(<?>) p l = label l p

-- The tokens the parser read, with its result.
match :: Stream s => ParsecT e s m a -> ParsecT e s m (Tokens s, a)
match p = do
  o <- getOffset
  s <- getInput
  r <- p
  o' <- getOffset
  return (fst (fromJust (takeN_ (o' - o) s)), r)

takeRest :: Stream s => ParsecT e s m (Tokens s)
takeRest = takeWhileP Nothing (const True)

atEnd :: Stream s => ParsecT e s m Bool
atEnd = option False (fmap (const True) (hidden eof))

-- ---- The state ----

getInput :: ParsecT e s m s
getInput = fmap stateInput getParserState

setInput :: s -> ParsecT e s m ()
setInput input = updateParserState (\s -> s {stateInput = input})

getOffset :: ParsecT e s m Int
getOffset = fmap stateOffset getParserState

setOffset :: Int -> ParsecT e s m ()
setOffset o = updateParserState (\s -> s {stateOffset = o})

setParserState :: State s e -> ParsecT e s m ()
setParserState st = updateParserState (const st)

getSourcePos :: TraversableStream s => ParsecT e s m SourcePos
getSourcePos = do
  st <- getParserState
  let pst = reachOffsetNoLine (stateOffset st) (statePosState st)
  setParserState st {statePosState = pst}
  return (pstateSourcePos pst)
