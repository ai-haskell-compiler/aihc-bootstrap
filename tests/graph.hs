module Main where

import Data.Array (Array, accumArray, array, assocs, bounds, elems, indices, listArray, (!))
import Data.Graph (SCC (..), flattenSCC, flattenSCCs, stronglyConnComp, stronglyConnCompR)
import qualified Data.Graph as Graph
import Data.Tree (Tree (..))

-- Printed by hand: `show` of a component differs between versions of
-- containers.
component :: Show a => SCC a -> String
component (AcyclicSCC a) = "acyclic " ++ show a
component (CyclicSCC as) = "cyclic " ++ show as

report :: (Show node, Ord key) => String -> [(node, key, [key])] -> IO ()
report name graph = do
  putStrLn name
  mapM_ (putStrLn . ("  " ++) . component) (stronglyConnComp graph)

tree :: Tree Int -> String
tree (Node v []) = show v
tree (Node v ts) = show v ++ "(" ++ unwords (map tree ts) ++ ")"

-- A pseudo-random graph: node i has edges to (i * a + c) mod n.
generated :: Int -> [(Int, Int, [Int])]
generated n = [(i * 10, i, [(i * 7 + 3) `mod` n, (i * i + 1) `mod` n, (i * 3) `mod` (n + 4)]) | i <- [0 .. n - 1]]

main :: IO ()
main = do
  let table = listArray (0, 4) "hello" :: Array Int Char
  print (bounds table)
  print (table ! 0, table ! 4)
  print (elems table)
  print (indices table)
  print (assocs (fmap succ table))
  print (elems (array (1, 3) [(3, 'c'), (1, 'a'), (2, 'b')] :: Array Int Char))
  print (assocs (accumArray (flip (:)) [] (0, 3) [(1, 'a'), (3, 'b'), (1, 'c'), (0, 'd'), (1, 'e')] :: Array Int String))
  print (assocs (accumArray (+) 0 (0, 2) [(0, 1), (2, 5), (0, 10)] :: Array Int Int))

  report "empty" ([] :: [(Int, Int, [Int])])
  report "documentation" [("a", 0 :: Int, [1]), ("b", 1, [2, 3]), ("c", 2, [1]), ("d", 3, [3])]
  report "chain" [(c, c, [succ c]) | c <- "abcde"]
  report "chain, listed backwards" [(c, c, [succ c]) | c <- "edcba"]
  report "one cycle" [(n, n, [(n + 1) `mod` 6]) | n <- [0 .. 5 :: Int]]
  report "one cycle, shuffled" [(n, n, [(n + 1) `mod` 6]) | n <- [3, 0, 5, 1, 4, 2 :: Int]]
  report "self loops" [(n, n, [n | even n]) | n <- [1 .. 6 :: Int]]
  report "edges to missing keys" [("x", "x", ["y", "nowhere"]), ("y", "y", ["elsewhere", "x"]), ("z", "z", ["x", "gone"])]
  report "two cycles joined" [(1 :: Int, 1 :: Int, [2]), (2, 2, [3]), (3, 3, [1, 4]), (4, 4, [5]), (5, 5, [6]), (6, 6, [4]), (7, 7, [1, 6])]
  report "repeated edges" [('a', 'a', "bbc"), ('b', 'b', "cca"), ('c', 'c', "c"), ('d', 'd', "aad")]
  report "tuple keys" [(show k, k, [(snd k, fst k)]) | k <- [(x, y) | x <- [1 .. 3 :: Int], y <- [1 .. 3 :: Int]]]
  report "generated 40" (generated 40)
  report "generated 97" (generated 97)

  print (flattenSCCs (stronglyConnComp (generated 25)))
  print (map flattenSCC (stronglyConnCompR [("a", 0 :: Int, [1]), ("b", 1, [0]), ("c", 2, [])]))

  let (graph, fromVertex, toVertex) = Graph.graphFromEdges (generated 12)
  print (assocs graph)
  print (Graph.edges (Graph.transposeG graph))
  print [key | v <- Graph.vertices graph, let (_, key, _) = fromVertex v]
  print (map toVertex [0, 5, 11, 12])
  putStrLn (unwords (map tree (Graph.dff graph)))
  putStrLn (unwords (map tree (Graph.dfs graph [7, 2])))
  putStrLn (unwords (map tree (Graph.scc graph)))
  print (Graph.topSort graph)
  print (Graph.reverseTopSort graph)
  print (Graph.buildG (0, 3) [(0, 1), (0, 2), (3, 0), (0, 3)] ! 0)
