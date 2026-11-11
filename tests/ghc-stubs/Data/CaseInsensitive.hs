-- Only for generating expected test output with GHC: upstream megaparsec
-- imports this module, and the package it is from is not shipped with GHC.
-- See tests/megaparsec.hs.
module Data.CaseInsensitive (FoldCase, mk) where

class FoldCase s

mk :: s -> s
mk = id
