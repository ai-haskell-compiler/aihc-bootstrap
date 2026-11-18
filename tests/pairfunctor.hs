module Main where

import Data.Foldable (toList)

-- Pairs and Either as containers of their last component.

main :: IO ()
main = do
  print (fmap (+ 1) ("a", 1 :: Int))
  print (toList ('x', True), sum ("s", 3 :: Int), length (Left 'e' :: Either Char Int))
  print (traverse (\n -> if n > 0 then Just n else Nothing) ("t", 2 :: Int))
  print (traverse (\n -> [n, n + 1]) ("t", 2 :: Int))
  print (traverse Just (Right 'r' :: Either Int Char), sequenceA (Left 1 :: Either Int [Char]))
  print (mapM (\(k, v) -> fmap ((,) k) v) [("a", Just 'x'), ("b", Just 'y')])
  print (sum (Right 4 :: Either String Int), toList (Left "no" :: Either String Int))
