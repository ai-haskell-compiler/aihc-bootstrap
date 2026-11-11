-- This file is derived from the package megaparsec, Copyright © 2015–present
-- Megaparsec contributors, and is under that package's licence: see
-- licenses/megaparsec.txt.

module Text.Megaparsec.State where

import Text.Megaparsec.Error (ParseError)
import Text.Megaparsec.Pos

-- Where a parser is.
data State s e = State
  { -- What is left of the input.
    stateInput :: s,
    -- How many tokens have been read.
    stateOffset :: !Int,
    -- Enough to find the line and column of an offset, for error messages.
    statePosState :: PosState s,
    -- Errors that did not stop the parser (registerParseError).
    stateParseErrors :: [ParseError s e]
  }

-- A place in the input whose line and column are known.
data PosState s = PosState
  { pstateInput :: s,
    pstateOffset :: !Int,
    pstateSourcePos :: !SourcePos,
    pstateTabWidth :: Pos,
    -- What is on the line before this place.
    pstateLinePrefix :: String
  }

initialState :: FilePath -> s -> State s e
initialState name input = State input 0 (initialPosState name input) []

initialPosState :: FilePath -> s -> PosState s
initialPosState name input = PosState input 0 (initialPos name) defaultTabWidth ""
