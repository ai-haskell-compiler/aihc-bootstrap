-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Internal where

-- The parser type and the parsers everything else is made of. The
-- definitions are upstream's, because which error a parser reports, and
-- whether it counts as having consumed input, decides what a parser built
-- from it does.
--
-- A parser is given the state and four continuations, and calls one:
--
--     cok   it succeeded and consumed input
--     cerr  it failed after consuming input
--     eok   it succeeded without consuming input
--     eerr  it failed without consuming input
--
-- Two differences from upstream. The continuations return `Answer`, where
-- upstream has `forall b. m b`: the interpreter has no such types, and the
-- one place that looks at an answer (runParsecT) knows what it is. And the
-- `m` of ParsecT is not used: there is no monad under the parser.

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as E
import Text.Megaparsec.Error
import Text.Megaparsec.State
import Text.Megaparsec.Stream

-- What could have come next at the place where a parser succeeded without
-- consuming input. They are added to a later error at the same place.
newtype Hints t = Hints (Set (ErrorItem t))

instance Semigroup (Hints t) where
  (<>) (Hints xs) (Hints ys) = Hints (E.union xs ys)

instance Monoid (Hints t) where
  mempty = Hints E.empty

data Reply e s a = Reply (State s e) Consumption (Result s e a)

data Consumption = Consumed | NotConsumed

data Result s e a
  = OK (Hints (Token s)) a
  | Error (ParseError s e)

-- What a continuation returns: a Reply, but only runParsecT knows.
data Answer

newtype ParsecT e s m a
  = ParsecT
      ( State s e ->
        (a -> State s e -> Hints (Token s) -> Answer) ->
        (ParseError s e -> State s e -> Answer) ->
        (a -> State s e -> Hints (Token s) -> Answer) ->
        (ParseError s e -> State s e -> Answer) ->
        Answer
      )

unParser ::
  ParsecT e s m a ->
  State s e ->
  (a -> State s e -> Hints (Token s) -> Answer) ->
  (ParseError s e -> State s e -> Answer) ->
  (a -> State s e -> Hints (Token s) -> Answer) ->
  (ParseError s e -> State s e -> Answer) ->
  Answer
unParser (ParsecT parser) = parser

toAnswer :: Reply e s a -> Answer
toAnswer = unsafeCoerce

fromAnswer :: Answer -> Reply e s a
fromAnswer = unsafeCoerce

runParsecT :: ParsecT e s m a -> State s e -> Reply e s a
runParsecT p s = fromAnswer (unParser p s cok cerr eok eerr)
  where
    cok a s' hs = toAnswer (Reply s' Consumed (OK hs a))
    cerr err s' = toAnswer (Reply s' Consumed (Error err))
    eok a s' hs = toAnswer (Reply s' NotConsumed (OK hs a))
    eerr err s' = toAnswer (Reply s' NotConsumed (Error err))

instance Functor (ParsecT e s m) where
  fmap f p = ParsecT (\s cok cerr eok eerr -> unParser p s (cok . f) cerr (eok . f) eerr)

instance Applicative (ParsecT e s m) where
  pure x = ParsecT (\s _ _ eok _ -> eok x s mempty)
  (<*>) m k = ParsecT (\s cok cerr eok eerr ->
    let mcok x s' hs = unParser k s' (cok . x) cerr (accHints hs (cok . x)) (withHints hs cerr)
        meok x s' hs = unParser k s' (cok . x) cerr (accHints hs (eok . x)) (withHints hs eerr)
     in unParser m s mcok cerr meok eerr)
  (*>) p1 p2 = p1 >>= \_ -> p2
  (<*) p1 p2 = p1 >>= \x -> p2 >>= \_ -> return x

instance Monad (ParsecT e s m) where
  (>>=) m k = ParsecT (\s cok cerr eok eerr ->
    let mcok x s' hs = unParser (k x) s' cok cerr (accHints hs cok) (withHints hs cerr)
        meok x s' hs = unParser (k x) s' cok cerr (accHints hs eok) (withHints hs eerr)
     in unParser m s mcok cerr meok eerr)

instance MonadFail (ParsecT e s m) where
  fail message = ParsecT (\s _ _ _ eerr ->
    eerr (FancyError (stateOffset s) (E.singleton (ErrorFail message))) s)

instance Alternative (ParsecT e s m) where
  empty = ParsecT (\s _ _ _ eerr -> eerr (TrivialError (stateOffset s) Nothing E.empty) s)
  (<|>) = pPlus

instance MonadPlus (ParsecT e s m) where
  mzero = empty
  mplus = pPlus

instance Semigroup a => Semigroup (ParsecT e s m a) where
  (<>) p1 p2 = p1 >>= \a -> fmap (\b -> a <> b) p2

instance Monoid a => Monoid (ParsecT e s m a) where
  mempty = pure mempty

-- The second parser runs only if the first one failed without consuming
-- input.
pPlus :: ParsecT e s m a -> ParsecT e s m a -> ParsecT e s m a
pPlus m n = ParsecT (\s cok cerr eok eerr ->
  let meerr err ms =
        let ncerr err' s' = cerr (err' <> err) (longestMatch ms s')
            neok x s' hs = eok x s' (toHints (stateOffset s') err <> hs)
            neerr err' s' = eerr (combineErrors (stateOffset s) err err') (longestMatch ms s')
         in unParser n s cok ncerr neok neerr
   in unParser m s cok cerr eok meerr)

combineErrors :: Int -> ParseError s e -> ParseError s e -> ParseError s e
combineErrors altOffset e1 e2 = case (e1, e2) of
  (TrivialError o1 u1 p1, TrivialError o2 u2 p2)
    | o1 == altOffset && o2 > altOffset -> TrivialError altOffset u1 p1
    | o2 == altOffset && o1 > altOffset -> TrivialError altOffset u2 p2
  _ -> e2 <> e1

longestMatch :: State s e -> State s e -> State s e
longestMatch s1 s2 = if stateOffset s1 > stateOffset s2 then s1 else s2

pParseError :: ParseError s e -> ParsecT e s m a
pParseError e = ParsecT (\s _ _ _ eerr -> eerr e s)

pLabel :: String -> ParsecT e s m a -> ParsecT e s m a
pLabel l p = ParsecT (\s cok cerr eok eerr ->
  let el = fmap Label (NE.nonEmpty l)
      cok' x s' hs = case el of
        Nothing -> cok x s' (refreshHints hs Nothing)
        Just _ -> cok x s' hs
      eok' x s' hs = eok x s' (refreshHints hs el)
      eerr' err = eerr (case err of
        TrivialError pos us _ -> TrivialError pos us (maybe E.empty E.singleton el)
        _ -> err)
   in unParser p s cok' cerr eok' eerr')

-- A failure of the parser counts as not having consumed input.
pTry :: ParsecT e s m a -> ParsecT e s m a
pTry p = ParsecT (\s cok _ eok eerr ->
  let eerr' err _ = eerr err s
   in unParser p s cok eerr' eok eerr')

pLookAhead :: ParsecT e s m a -> ParsecT e s m a
pLookAhead p = ParsecT (\s _ cerr eok eerr ->
  let eok' a _ _ = eok a s mempty
   in unParser p s eok' cerr eok' eerr)

pNotFollowedBy :: Stream s => ParsecT e s m a -> ParsecT e s m ()
pNotFollowedBy p = ParsecT (\s _ _ eok eerr ->
  let what = maybe EndOfInput (\found -> Tokens (nes (fst found))) (take1_ (stateInput s))
      unexpected u = TrivialError (stateOffset s) (Just u) E.empty
      cok' _ _ _ = eerr (unexpected what) s
      cerr' _ _ = eok () s mempty
      eok' _ _ _ = eerr (unexpected what) s
      eerr' _ _ = eok () s mempty
   in unParser p s cok' cerr' eok' eerr')

pWithRecovery :: (ParseError s e -> ParsecT e s m a) -> ParsecT e s m a -> ParsecT e s m a
pWithRecovery r p = ParsecT (\s cok cerr eok eerr ->
  let mcerr err ms =
        let rcok x s' _ = cok x s' mempty
            rcerr _ _ = cerr err ms
            reok x s' _ = eok x s' (toHints (stateOffset s') err)
            reerr _ _ = cerr err ms
         in unParser (r err) ms rcok rcerr reok reerr
      meerr err ms =
        let rcok x s' _ = cok x s' (toHints (stateOffset s') err)
            rcerr _ _ = eerr err ms
            reok x s' _ = eok x s' (toHints (stateOffset s') err)
            reerr _ _ = eerr err ms
         in unParser (r err) ms rcok rcerr reok reerr
   in unParser p s cok mcerr eok meerr)

pObserving :: ParsecT e s m a -> ParsecT e s m (Either (ParseError s e) a)
pObserving p = ParsecT (\s cok _ eok _ ->
  let cerr' err s' = cok (Left err) s' mempty
      eerr' err s' = eok (Left err) s' (toHints (stateOffset s') err)
   in unParser p s (cok . Right) cerr' (eok . Right) eerr')

pEof :: Stream s => ParsecT e s m ()
pEof = ParsecT (\s _ _ eok eerr -> case take1_ (stateInput s) of
  Nothing -> eok () s mempty
  Just (x, _) -> eerr (TrivialError (stateOffset s) (Just (Tokens (nes x))) (E.singleton EndOfInput)) s)

pToken :: Stream s => (Token s -> Maybe a) -> Set (ErrorItem (Token s)) -> ParsecT e s m a
pToken test ps = ParsecT (\s cok _ _ eerr -> case s of
  State input o pst de -> case take1_ input of
    Nothing -> eerr (TrivialError o (Just EndOfInput) ps) s
    Just (c, cs) -> case test c of
      Nothing -> eerr (TrivialError o (Just (Tokens (nes c))) ps) s
      Just x -> cok x (State cs (o + 1) pst de) mempty)

pTokens :: Stream s => (Tokens s -> Tokens s -> Bool) -> Tokens s -> ParsecT e s m (Tokens s)
pTokens f tts = ParsecT (\s cok _ eok eerr -> case s of
  State input o pst de ->
    let pxy = proxyOf input
        unexpected u = TrivialError o (Just u) (E.singleton (Tokens (NE.fromList (chunkToTokens pxy tts))))
        len = chunkLength pxy tts
     in case takeN_ len input of
          Nothing -> eerr (unexpected EndOfInput) s
          Just (tts', input') ->
            if f tts tts'
              then
                let st = State input' (o + len) pst de
                 in if chunkEmpty pxy tts then eok tts' st mempty else cok tts' st mempty
              else eerr (unexpected (Tokens (NE.fromList (chunkToTokens pxy tts')))) s)

pTakeWhileP :: Stream s => Maybe String -> (Token s -> Bool) -> ParsecT e s m (Tokens s)
pTakeWhileP ml f = ParsecT (\s cok _ eok _ -> case s of
  State input o pst de ->
    let pxy = proxyOf input
        (ts, input') = takeWhile_ f input
        len = chunkLength pxy ts
        hs = case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> Hints (E.singleton (Label l))
     in if chunkEmpty pxy ts
          then eok ts (State input' (o + len) pst de) hs
          else cok ts (State input' (o + len) pst de) hs)

pTakeWhile1P :: Stream s => Maybe String -> (Token s -> Bool) -> ParsecT e s m (Tokens s)
pTakeWhile1P ml f = ParsecT (\s cok _ _ eerr -> case s of
  State input o pst de ->
    let pxy = proxyOf input
        (ts, input') = takeWhile_ f input
        len = chunkLength pxy ts
        el = fmap Label (ml >>= NE.nonEmpty)
        hs = case el of
          Nothing -> mempty
          Just l -> Hints (E.singleton l)
     in if chunkEmpty pxy ts
          then
            let us = case take1_ input of
                  Nothing -> EndOfInput
                  Just (t, _) -> Tokens (nes t)
             in eerr (TrivialError o (Just us) (maybe E.empty E.singleton el)) s
          else cok ts (State input' (o + len) pst de) hs)

pTakeP :: Stream s => Maybe String -> Int -> ParsecT e s m (Tokens s)
pTakeP ml n' = ParsecT (\s cok _ eok eerr -> case s of
  State input o pst de ->
    let n = max 0 n'
        pxy = proxyOf input
        ps = maybe E.empty E.singleton (fmap Label (ml >>= NE.nonEmpty))
     in case takeN_ n input of
          Nothing -> eerr (TrivialError o (Just EndOfInput) ps) s
          Just (ts, input') ->
            let len = chunkLength pxy ts
                st = State input' (o + len) pst de
             in if len /= n
                  then eerr (TrivialError (o + len) (Just EndOfInput) ps) s
                  else if chunkEmpty pxy ts then eok ts st mempty else cok ts st mempty)

pGetParserState :: ParsecT e s m (State s e)
pGetParserState = ParsecT (\s _ _ eok _ -> eok s s mempty)

pUpdateParserState :: (State s e -> State s e) -> ParsecT e s m ()
pUpdateParserState f = ParsecT (\s _ _ eok _ -> eok () (f s) mempty)

nes :: a -> NonEmpty a
nes x = x :| []

-- What the error expected, if it is at the place the parser is at.
toHints :: Int -> ParseError s e -> Hints (Token s)
toHints streamPos err = case err of
  TrivialError errOffset _ ps -> if streamPos == errOffset then Hints ps else mempty
  FancyError _ _ -> mempty

-- Adds the hints to what an error expected.
withHints ::
  Hints (Token s) ->
  (ParseError s e -> State s e -> Answer) ->
  ParseError s e ->
  State s e ->
  Answer
withHints (Hints ps') c e = case e of
  TrivialError pos us ps -> c (TrivialError pos us (E.union ps ps'))
  _ -> c e

-- Adds the hints to those of a success.
accHints ::
  Hints (Token s) ->
  (a -> State s e -> Hints (Token s) -> Answer) ->
  a ->
  State s e ->
  Hints (Token s) ->
  Answer
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)

refreshHints :: Hints t -> Maybe (ErrorItem t) -> Hints t
refreshHints (Hints hs) item = case item of
  Nothing -> Hints E.empty
  Just m -> if E.null hs then Hints hs else Hints (E.singleton m)

foreign import prim "unsafeCoerce" unsafeCoerce :: a -> b
