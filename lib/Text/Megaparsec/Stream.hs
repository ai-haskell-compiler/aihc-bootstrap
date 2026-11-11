-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Stream where

import Data.Char (chr)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec.Pos
import Text.Megaparsec.State

-- What a parser reads from: something that gives one token after another.
-- A chunk is several tokens together, in the form that suits the stream.
class Stream s where
  type Token s
  type Tokens s
  tokenToChunk :: Proxy s -> Token s -> Tokens s
  tokenToChunk pxy t = tokensToChunk pxy [t]
  tokensToChunk :: Proxy s -> [Token s] -> Tokens s
  chunkToTokens :: Proxy s -> Tokens s -> [Token s]
  chunkLength :: Proxy s -> Tokens s -> Int
  chunkEmpty :: Proxy s -> Tokens s -> Bool
  chunkEmpty pxy ts = chunkLength pxy ts <= 0

  -- The first token and the rest, unless the stream is empty.
  take1_ :: s -> Maybe (Token s, s)

  -- The first `n` tokens, or as many as there are. Nothing if tokens were
  -- asked for and the stream is empty.
  takeN_ :: Int -> s -> Maybe (Tokens s, s)

  -- The tokens up to the first one that fails the test.
  takeWhile_ :: (Token s -> Bool) -> s -> (Tokens s, s)

instance Stream [a] where
  type Token [a] = a
  type Tokens [a] = [a]
  tokenToChunk _ t = [t]
  tokensToChunk _ ts = ts
  chunkToTokens _ ts = ts
  chunkLength _ ts = length ts
  chunkEmpty _ ts = null ts
  take1_ list = case list of
    [] -> Nothing
    t : ts -> Just (t, ts)
  takeN_ n list
    | n <= 0 = Just ([], list)
    | null list = Nothing
    | otherwise = Just (splitAt n list)
  takeWhile_ = span

instance Stream Text where
  type Token Text = Char
  type Tokens Text = Text
  tokenToChunk _ t = T.singleton t
  tokensToChunk _ ts = T.pack ts
  chunkToTokens _ ts = T.unpack ts
  chunkLength _ ts = T.length ts
  chunkEmpty _ ts = T.null ts
  take1_ = T.uncons
  takeN_ n text
    | n <= 0 = Just (T.empty, text)
    | T.null text = Nothing
    | otherwise = Just (T.splitAt n text)
  takeWhile_ = T.span

-- The proxy for the type of a stream at hand.
proxyOf :: s -> Proxy s
proxyOf _ = Proxy

-- ---- Showing tokens in error messages ----

class Stream s => VisualStream s where
  showTokens :: Proxy s -> NonEmpty (Token s) -> String

  -- How many columns the tokens take.
  tokensLength :: Proxy s -> NonEmpty (Token s) -> Int
  tokensLength _ ts = NE.length ts

instance VisualStream [Char] where
  showTokens _ = stringPretty
  tokensLength _ ts = stringWidth (NE.toList ts)

instance VisualStream Text where
  showTokens _ = stringPretty
  tokensLength _ ts = stringWidth (NE.toList ts)

-- How many columns the string takes on a terminal. Control characters take
-- none. Upstream also knows the characters that take two (most of the
-- East Asian ones); here every other character takes one, which only
-- changes how long the pointer under an error is.
stringWidth :: String -> Int
stringWidth string = length (filter (not . zeroWidth) string)
  where
    zeroWidth c = c < ' ' || (c >= '\DEL' && c <= chr 159) || c == chr 173

stringPretty :: NonEmpty Char -> String
stringPretty (c :| rest) = case rest of
  [] -> charPretty c
  _ ->
    if c : rest == "\r\n"
      then "crlf newline"
      else "\"" ++ concatMap inString (c : rest) ++ "\""
  where
    inString ch = case charName ch of
      Nothing -> [ch]
      Just name -> "<" ++ name ++ ">"

charPretty :: Char -> String
charPretty c = case c of
  ' ' -> "space"
  _ -> case charName c of
    Nothing -> "'" ++ [c] ++ "'"
    Just name -> name

-- The name of a character that cannot be shown as itself.
charName :: Char -> Maybe String
charName c = lookup c names
  where
    names =
      [ ('\0', "null"),
        ('\t', "tab"),
        ('\n', "newline"),
        ('\r', "carriage return"),
        ('\ESC', "escape"),
        ('\DEL', "delete"),
        (chr 160, "non-breaking space")
      ]

-- ---- Finding the line and column of an offset ----

class Stream s => TraversableStream s where
  -- The line that the offset is on, and the place of the offset.
  reachOffset :: Int -> PosState s -> (Maybe String, PosState s)

  reachOffsetNoLine :: Int -> PosState s -> PosState s
  reachOffsetNoLine offset pst = snd (reachOffset offset pst)

instance TraversableStream [Char] where
  reachOffset = reachOffsetWith splitAt id

instance TraversableStream Text where
  reachOffset = reachOffsetWith (\n text -> case T.splitAt n text of (a, b) -> (T.unpack a, b)) T.unpack

-- Walks from the place the state is at to the offset, counting lines and
-- columns. The functions split off a number of characters and give the
-- characters of a stream.
reachOffsetWith ::
  (Int -> s -> (String, s)) ->
  (s -> String) ->
  Int ->
  PosState s ->
  (Maybe String, PosState s)
reachOffsetWith split characters offset pst =
  (if null line then Nothing else Just line, PosState rest (max offset (pstateOffset pst)) position (pstateTabWidth pst) prefix)
  where
    (consumed, rest) = split (offset - pstateOffset pst) (pstateInput pst)
    (position, prefix) = foldl step (pstateSourcePos pst, pstateLinePrefix pst) consumed
    line = concatMap expandTab (prefix ++ takeWhile (/= '\n') (characters rest))
    width = unPos (pstateTabWidth pst)
    expandTab c = if c == '\t' then replicate width ' ' else [c]
    step (SourcePos name l c, before) ch = case ch of
      '\n' -> (SourcePos name (l <> pos1) pos1, "")
      '\t' -> (SourcePos name l (mkPos (unPos c + width - ((unPos c - 1) `rem` width))), before ++ [ch])
      _ -> (SourcePos name l (c <> pos1), before ++ [ch])
