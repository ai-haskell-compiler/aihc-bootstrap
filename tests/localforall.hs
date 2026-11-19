{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Data (Data, gmapQ)
import Data.Typeable (eqT, (:~:) (Refl))

data Tree = Leaf Int | Node Tree String Tree
  deriving (Data)

-- A local function whose signature has a forall: its type variable is in
-- scope in its definition, so eqT can be asked about it.
numbers :: Tree -> [Int]
numbers = go
  where
    go :: forall value. (Data value) => value -> [Int]
    go value =
      case eqT @value @Int of
        Just Refl -> [value]
        Nothing -> concat (gmapQ go value)

main :: IO ()
main = print (numbers (Node (Node (Leaf 1) "a" (Leaf 2)) "b" (Leaf 3)))
