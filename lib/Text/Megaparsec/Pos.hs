-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.Pos where

-- A line or column number: at least 1.
newtype Pos = Pos Int
  deriving (Eq, Ord, Show)

mkPos :: Int -> Pos
mkPos n = if n <= 0 then error "Text.Megaparsec.Pos.mkPos: not a positive number" else Pos n

unPos :: Pos -> Int
unPos (Pos n) = n

pos1 :: Pos
pos1 = Pos 1

defaultTabWidth :: Pos
defaultTabWidth = Pos 8

instance Semigroup Pos where
  (<>) (Pos a) (Pos b) = Pos (a + b)

data SourcePos = SourcePos
  { sourceName :: FilePath,
    sourceLine :: !Pos,
    sourceColumn :: !Pos
  }
  deriving (Eq, Ord, Show)

initialPos :: FilePath -> SourcePos
initialPos name = SourcePos name pos1 pos1

sourcePosPretty :: SourcePos -> String
sourcePosPretty (SourcePos name line column) =
  (if null name then "" else name ++ ":") ++ show (unPos line) ++ ":" ++ show (unPos column)
