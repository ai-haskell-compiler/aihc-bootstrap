{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Main where

-- A record whose values are put together field by field.

import qualified Data.Map as Map
import GHC.Generics (Generic, Generically (..))

data Totals = Totals [Int] (Map.Map String Int) String
  deriving stock (Eq, Show, Generic)
  deriving (Semigroup, Monoid) via Generically Totals

main :: IO ()
main = do
  let a = Totals [1] (Map.fromList [("x", 1), ("y", 2)]) "ab"
      b = Totals [2, 3] (Map.fromList [("x", 9), ("z", 3)]) "c"
  print (a <> b)
  print (mempty :: Totals)
  print (mconcat [a, b, a] == a <> (b <> a), mappend a mempty == a, mempty <> b == b)
