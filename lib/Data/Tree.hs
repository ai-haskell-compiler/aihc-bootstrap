module Data.Tree where

data Tree a = Node a [Tree a]

type Forest a = [Tree a]

rootLabel :: Tree a -> a
rootLabel (Node a _) = a

subForest :: Tree a -> [Tree a]
subForest (Node _ trees) = trees

-- The labels, each before those of its subtrees.
flatten :: Tree a -> [a]
flatten (Node a trees) = a : concatMap flatten trees

instance Functor Tree where
  fmap f (Node a trees) = Node (f a) (map (fmap f) trees)
