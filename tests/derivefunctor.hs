{-# LANGUAGE DeriveTraversable #-}

module Main where

-- Derived Functor, Foldable and Traversable: the values of the last type
-- parameter, wherever they are in the fields.

import Data.Foldable (toList)

-- A first operand and pairs of an operator and an operand, as AIHC has.
data Chain op a = Chain a [(op, a)]
  deriving (Show, Functor, Foldable, Traversable)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

data Shape n a = Circle n | Labelled String a | Both (Maybe a) (Either n a) | Many [[a]]
  deriving (Show, Functor, Foldable, Traversable)

newtype Wrap a = Wrap (Maybe a)
  deriving (Show, Functor, Foldable, Traversable)

chain :: Chain Char Int
chain = Chain 1 [('+', 2), ('*', 3)]

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

positive :: Int -> Maybe Int
positive n = if n > 0 then Just n else Nothing

main :: IO ()
main = do
  print (fmap (* 10) chain, sum chain, toList chain, length chain)
  print (traverse positive chain, traverse (\n -> positive (n - 2)) chain)
  print (fmap show tree, toList tree, maximum tree, elem 3 tree, null Leaf)
  print (traverse (\n -> [n, n + 10]) (Node Leaf 1 (Node Leaf 2 Leaf)))
  print (map (fmap not) [Circle 'x', Labelled "l" True, Both (Just False) (Right True), Both Nothing (Left 'n'), Many [[True], [], [False, True]]])
  print (map toList [Circle 'x', Labelled "l" 1, Both (Just 2) (Right 3), Both Nothing (Left 'n'), Many [[4], [], [5, 6 :: Int]]])
  print (traverse positive (Many [[1, 2], [3]] :: Shape () Int), traverse positive (Both (Just 1) (Right 0) :: Shape () Int))
  print (fmap (+ 1) (Wrap (Just 1)), toList (Wrap (Just 'c')), traverse positive (Wrap (Just 0)), 7 <$ chain)
  mapM_ print chain
