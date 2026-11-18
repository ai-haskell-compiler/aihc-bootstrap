{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Data (Data, gmapM)
import Data.Typeable (eqT, (:~:) (Refl))

data Tree = Leaf Int | Node Tree String Tree
  deriving (Data, Show)

-- The variable of the outer signature and that of the local one together.
-- The signature says what the case gives: m value, not the m Int of its
-- first alternative.
visit :: forall m. (Monad m) => (Int -> m Int) -> Tree -> m Tree
visit onNumber = go
  where
    go :: forall value. (Data value) => value -> m value
    go value =
      case eqT @value @Int of
        Just Refl -> number value
        Nothing -> gmapM go value
    number :: Int -> m Int
    number n = onNumber n

main :: IO ()
main = do
  tree <- visit (\n -> do print n; pure (n * 10)) (Node (Leaf 1) "a" (Leaf 2))
  print tree
