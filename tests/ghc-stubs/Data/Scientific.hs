-- Only for generating expected test output with GHC: upstream megaparsec
-- imports this module, and the package it is from is not shipped with GHC.
-- See tests/megaparsec.hs.
module Data.Scientific (Scientific, scientific, toRealFloat) where

data Scientific = Scientific Integer Int

scientific :: Integer -> Int -> Scientific
scientific = Scientific

-- Exact, as the real package is: the nearest number to c * 10^e.
toRealFloat :: RealFloat a => Scientific -> a
toRealFloat (Scientific c e) = fromRational (fromInteger c * 10 ^^ e)
