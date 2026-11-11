-- Only for generating expected test output with GHC: upstream megaparsec
-- imports this module, and the package it is from is not shipped with GHC.
-- See tests/megaparsec.hs.
module Data.Scientific (Scientific, scientific, toRealFloat) where

data Scientific = Scientific Integer Int

scientific :: Integer -> Int -> Scientific
scientific = Scientific

toRealFloat :: RealFloat a => Scientific -> a
toRealFloat (Scientific c e) = fromInteger c * 10 ^^ e
