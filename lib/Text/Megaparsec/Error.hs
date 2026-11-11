-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Error where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as E
import Data.Void (Void, absurd)
import Text.Megaparsec.Pos
import Text.Megaparsec.State
import Text.Megaparsec.Stream

-- What was found or expected at the place of an error.
data ErrorItem t
  = Tokens (NonEmpty t)
  | Label (NonEmpty Char)
  | EndOfInput
  deriving (Eq, Ord, Show)

instance Functor ErrorItem where
  fmap f item = case item of
    Tokens ts -> Tokens (NE.map f ts)
    Label l -> Label l
    EndOfInput -> EndOfInput

-- An error other than "this was found, that was expected".
data ErrorFancy e
  = ErrorFail String
  | ErrorIndentation Ordering Pos Pos
  | ErrorCustom e
  deriving (Eq, Ord, Show)

instance Functor ErrorFancy where
  fmap f fancy = case fancy of
    ErrorFail message -> ErrorFail message
    ErrorIndentation ordering reference actual -> ErrorIndentation ordering reference actual
    ErrorCustom e -> ErrorCustom (f e)

-- An error at an offset: what was found there (if anything) and what was
-- expected, or a set of other errors.
data ParseError s e
  = TrivialError Int (Maybe (ErrorItem (Token s))) (Set (ErrorItem (Token s)))
  | FancyError Int (Set (ErrorFancy e))
  deriving (Eq, Show)

instance Semigroup (ParseError s e) where
  (<>) = mergeError

instance Monoid (ParseError s e) where
  mempty = TrivialError 0 Nothing E.empty

mapParseError :: (e -> e') -> ParseError s e -> ParseError s e'
mapParseError f err = case err of
  TrivialError o u p -> TrivialError o u p
  FancyError o x -> FancyError o (E.map (fmap f) x)

errorOffset :: ParseError s e -> Int
errorOffset err = case err of
  TrivialError o _ _ -> o
  FancyError o _ -> o

setErrorOffset :: Int -> ParseError s e -> ParseError s e
setErrorOffset o err = case err of
  TrivialError _ u p -> TrivialError o u p
  FancyError _ x -> FancyError o x

-- The error that is further into the input wins. Errors at the same place
-- are put together, and a fancy one wins over a trivial one.
mergeError :: ParseError s e -> ParseError s e -> ParseError s e
mergeError e1 e2 = case compare (errorOffset e1) (errorOffset e2) of
  LT -> e2
  GT -> e1
  EQ -> case e1 of
    TrivialError s1 u1 p1 -> case e2 of
      TrivialError _ u2 p2 -> TrivialError s1 (unexpected u1 u2) (E.union p1 p2)
      FancyError _ _ -> e2
    FancyError s1 x1 -> case e2 of
      TrivialError _ _ _ -> e1
      FancyError _ x2 -> FancyError s1 (E.union x1 x2)
  where
    unexpected Nothing Nothing = Nothing
    unexpected (Just x) Nothing = Just x
    unexpected Nothing (Just y) = Just y
    unexpected (Just x) (Just y) = Just (max x y)

-- The errors of one run of a parser, sorted by offset, and what is needed
-- to find their lines and columns.
data ParseErrorBundle s e = ParseErrorBundle
  { bundleErrors :: NonEmpty (ParseError s e),
    bundlePosState :: PosState s
  }

-- A custom error that can be shown.
class ShowErrorComponent a where
  showErrorComponent :: a -> String
  errorComponentLen :: a -> Int
  errorComponentLen _ = 1

instance ShowErrorComponent Void where
  showErrorComponent = absurd

-- ---- Showing errors ----

-- Every error with its place and the line it is on:
--
--     name:1:5:
--       |
--     1 | the line
--       |     ^
--     unexpected 'l'
--     expecting digit
errorBundlePretty ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  ParseErrorBundle s e ->
  String
errorBundlePretty (ParseErrorBundle errors posState) = go posState (NE.toList errors)
  where
    go _ [] = ""
    go pst (err : rest) = case reachOffset (errorOffset err) pst of
      (line, reached) -> one line (pstateSourcePos reached) err ++ separator rest ++ go reached rest
    separator rest = if null rest then "" else "\n"
    one line position err =
      sourcePosPretty position ++ ":\n"
        ++ padding ++ "|\n"
        ++ number ++ " | " ++ shownLine ++ "\n"
        ++ padding ++ "| " ++ replicate shift ' ' ++ replicate pointerLength '^' ++ "\n"
        ++ parseErrorTextPretty err
      where
        number = show (unPos (sourceLine position))
        padding = replicate (length number + 1) ' '
        shownLine = case line of
          Nothing -> "<empty line>"
          Just text -> text
        shift = unPos (sourceColumn position) - 1
        -- The pointer is as long as what was found, but stays on the line.
        pointerLength =
          max 1 (if shift + errorLength err > stringWidth shownLine
            then stringWidth shownLine - shift + 1
            else errorLength err)

-- How many columns what the error found takes.
errorLength :: (VisualStream s, ShowErrorComponent e) => ParseError s e -> Int
errorLength err = case err of
  TrivialError _ (Just (Tokens ts)) _ -> tokensLength (proxyOfError err) ts
  TrivialError _ _ _ -> 1
  FancyError _ xs -> foldl max 1 (map fancyLength (E.toAscList xs))
  where
    fancyLength fancy = case fancy of
      ErrorCustom e -> errorComponentLen e
      _ -> 1

parseErrorPretty :: (VisualStream s, ShowErrorComponent e) => ParseError s e -> String
parseErrorPretty err = "offset=" ++ show (errorOffset err) ++ ":\n" ++ parseErrorTextPretty err

-- The error without its place. It ends with a newline.
parseErrorTextPretty :: (VisualStream s, ShowErrorComponent e) => ParseError s e -> String
parseErrorTextPretty err = case err of
  TrivialError _ us ps ->
    if us == Nothing && E.null ps
      then "unknown parse error\n"
      else
        messageItemsPretty "unexpected " (map (showErrorItem (proxyOfError err)) (maybe [] (\u -> [u]) us))
          ++ messageItemsPretty "expecting " (map (showErrorItem (proxyOfError err)) (E.toAscList ps))
  FancyError _ xs ->
    if E.null xs
      then "unknown fancy parse error\n"
      else unlines (map showErrorFancy (E.toAscList xs))

proxyOfError :: ParseError s e -> Proxy s
proxyOfError _ = Proxy

showErrorItem :: VisualStream s => Proxy s -> ErrorItem (Token s) -> String
showErrorItem pxy item = case item of
  Tokens ts -> showTokens pxy ts
  Label label -> NE.toList label
  EndOfInput -> "end of input"

showErrorFancy :: ShowErrorComponent e => ErrorFancy e -> String
showErrorFancy fancy = case fancy of
  ErrorFail message -> message
  ErrorIndentation ordering reference actual ->
    "incorrect indentation (got " ++ show (unPos actual) ++ ", should be " ++ relation ++ show (unPos reference) ++ ")"
    where
      relation = case ordering of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom e -> showErrorComponent e

-- The items after the prefix, as "a", "a or b" or "a, b, or c". Upstream
-- sorts the shown items and drops repeated ones.
messageItemsPretty :: String -> [String] -> String
messageItemsPretty prefix items = case E.toAscList (E.fromList items) of
  [] -> ""
  sorted -> prefix ++ orList sorted ++ "\n"

orList :: [String] -> String
orList items = case items of
  [x] -> x
  [x, y] -> x ++ " or " ++ y
  _ -> intercalate ", " (init items) ++ ", or " ++ last items
