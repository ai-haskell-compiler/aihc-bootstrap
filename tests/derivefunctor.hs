{-# LANGUAGE DeriveTraversable #-}

module Main where

import Data.Foldable (toList)

-- Derived Functor, Foldable and Traversable. AIHC derives them for
-- InfixChain in aihc-resolve, which has the shape of Chain.

data Chain op a = Chain a [(op, a)]
  deriving (Show, Functor, Foldable, Traversable)

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show, Functor, Foldable, Traversable)

newtype Box a = Box (Maybe (Int, a))
  deriving (Show, Functor, Foldable, Traversable)

data Pair b a = Pair b a a String
  deriving (Show, Functor, Foldable, Traversable)

chain :: Chain Char Int
chain = Chain 1 [('+', 2), ('*', 3)]

tree :: Tree Int
tree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 (Node Leaf 4 Leaf))

halve :: Int -> Either String Int
halve n
  | even n = Right (n `div` 2)
  | otherwise = Left ("odd: " ++ show n)

main :: IO ()
main = do
  print (fmap (* 10) chain)
  print (toList chain, sum chain, length chain, null chain, maximum chain)
  print (traverse (\n -> if n > 0 then Just (show n) else Nothing) chain)
  print (traverse halve chain, traverse halve (fmap (* 2) chain))
  print (fmap show tree, foldr (:) [] tree, length (Leaf :: Tree Int))
  print (traverse halve (fmap (* 2) tree))
  print (fmap negate (Box (Just (7, 8))), toList (Box Nothing :: Box Int))
  print (fmap length (Pair True "ab" "cde" "kept"), toList (Pair 'x' 1 2 "s"))
  print ('z' <$ chain)
  result <- traverse (\n -> do print n; return (n + 1)) tree
  print result
  mapM_ print chain
  print (sequenceA (Chain [1, 2] [('a', [3])]))
