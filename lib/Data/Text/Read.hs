module Data.Text.Read where

-- Numbers at the start of a text.

import Data.Char (isDigit, ord)
import Data.Text (Text)
import qualified Data.Text as T

type Reader a = Text -> Either String (a, Text)

-- The digits at the start of the text as a number, and the rest of the
-- text.
decimal :: Integral a => Reader a
decimal text =
  let digits = T.takeWhile isDigit text
   in if T.null digits
        then Left "input does not start with a digit"
        else
          Right
            ( T.foldl' (\total c -> total * 10 + fromIntegral (ord c - 48)) 0 digits,
              T.drop (T.length digits) text
            )

-- The same with a sign in front.
signed :: Num a => Reader a -> Reader a
signed reader text = case T.uncons text of
  Just ('-', rest) -> fmap (\(a, after) -> (negate a, after)) (reader rest)
  Just ('+', rest) -> reader rest
  _ -> reader text
