module Main where

import Data.List (foldl')

-- Makes far more values than it keeps. It only runs in the memory it has if
-- what is no longer needed is freed, and it only prints the right answers
-- if what is still needed is not. `make test-gc` runs it, with every other
-- test, with a collection after every 100 kB.

-- A list that is kept while everything below is made and thrown away.
table :: [(Int, String)]
table = [(n, show n) | n <- [1 .. 20000]]

main :: IO ()
main = do
  print (length (concatMap snd table))
  print (foldl' (+) 0 [1 .. 200000 :: Int])
  print (foldl' (\total n -> total + length (show n)) 0 [1 .. 40000 :: Int])
  print (sum (map fst table))
  print (snd (table !! 12344))
