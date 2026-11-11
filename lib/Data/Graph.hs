-- This file is derived from Data.Graph in the package containers,
-- Copyright 2004, The University Court of the University of Glasgow, and is
-- under that package's licence: see licenses/containers.txt.

module Data.Graph where

-- Strongly connected components, as in containers 0.7.
--
-- The order of the components, and of the vertices inside one, reaches the
-- code AIHC generates. So this follows the upstream definitions line by
-- line (graphFromEdges, transposeG, postOrd, dfs, scc, stronglyConnCompR),
-- with two differences that cannot be seen in the result:
--
--   * the vertices visited so far are kept in a set that is passed along,
--     where upstream uses a mutable array;
--   * a cyclic component holds a list, as before containers 0.7, where
--     upstream now holds a non-empty list behind a pattern synonym with the
--     same name. `show` of a component therefore prints differently.

import Data.Array
import qualified Data.IntSet as IntSet
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Tree (Tree (..))

data SCC vertex
  = AcyclicSCC vertex
  | CyclicSCC [vertex]
  deriving (Eq, Show)

instance Functor SCC where
  fmap f (AcyclicSCC v) = AcyclicSCC (f v)
  fmap f (CyclicSCC vs) = CyclicSCC (map f vs)

flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

flattenSCC :: SCC vertex -> [vertex]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (CyclicSCC vs) = vs

-- The components in reverse topological order: a component comes after
-- every component it has an edge to.
stronglyConnComp :: Ord key => [(node, key, [key])] -> [SCC node]
stronglyConnComp edges0 = map (fmap getNode) (stronglyConnCompR edges0)
  where
    getNode (n, _, _) = n

stronglyConnCompR :: Ord key => [(node, key, [key])] -> [SCC (node, key, [key])]
stronglyConnCompR [] = []
stronglyConnCompR edges0 = map decode forest
  where
    (graph, vertexFn, _) = graphFromEdges edges0
    forest = scc graph

    decode (Node v [])
      | mentionsItself v = CyclicSCC [vertexFn v]
      | otherwise = AcyclicSCC (vertexFn v)
    decode (Node v ts) = CyclicSCC (vertexFn v : foldr dec [] ts)

    dec (Node v ts) vs = vertexFn v : foldr dec vs ts
    mentionsItself v = v `elem` (graph ! v)

type Vertex = Int

type Graph = Array Vertex [Vertex]

type Bounds = (Vertex, Vertex)

type Edge = (Vertex, Vertex)

vertices :: Graph -> [Vertex]
vertices = indices

edges :: Graph -> [Edge]
edges g = [(v, w) | v <- vertices g, w <- g ! v]

buildG :: Bounds -> [Edge] -> Graph
buildG = accumArray (flip (:)) []

transposeG :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE :: Graph -> [Edge]
reverseE g = [(w, v) | (v, w) <- edges g]

-- The vertices are the nodes numbered in the order of their keys.
graphFromEdges ::
  Ord key =>
  [(node, key, [key])] ->
  (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
graphFromEdges edges0 = (graph, \v -> vertexMap ! v, keyVertex)
  where
    maxV = length edges0 - 1
    bounds0 = (0, maxV)
    sortedEdges = sortBy lt edges0
    edges1 = zip [0 ..] sortedEdges

    graph = array bounds0 [(v, mapMaybe keyVertex ks) | (v, (_, _, ks)) <- edges1]
    keyMap = array bounds0 [(v, k) | (v, (_, k, _)) <- edges1]
    vertexMap = array bounds0 edges1

    lt (_, k1, _) (_, k2, _) = compare k1 k2

    -- Nothing for a key that is not the key of a node.
    keyVertex k = findVertex 0 maxV
      where
        findVertex a b
          | a > b = Nothing
          | otherwise =
              let mid = a + (b - a) `div` 2
               in case compare k (keyMap ! mid) of
                    LT -> findVertex a (mid - 1)
                    EQ -> Just mid
                    GT -> findVertex (mid + 1) b

-- A spanning forest of the graph, searching depth first from every vertex.
dff :: Graph -> [Tree Vertex]
dff g = dfs g (vertices g)

-- A spanning forest of the part of the graph that can be reached from the
-- listed vertices, searching depth first from each of them in order.
dfs :: Graph -> [Vertex] -> [Tree Vertex]
dfs g vs0 = fst (go vs0 IntSet.empty)
  where
    go [] visited = ([], visited)
    go (v : vs) visited
      | IntSet.member v visited = go vs visited
      | otherwise = case go (g ! v) (IntSet.insert v visited) of
          (as, visited1) -> case go vs visited1 of
            (bs, visited2) -> (Node v as : bs, visited2)

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF :: [Tree a] -> [a] -> [a]
postorderF ts = foldr (.) id (map postorder ts)

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g) []

topSort :: Graph -> [Vertex]
topSort = reverse . postOrd

reverseTopSort :: Graph -> [Vertex]
reverseTopSort = postOrd

-- The strongly connected components, reverse topologically sorted.
scc :: Graph -> [Tree Vertex]
scc g = dfs g (reverse (postOrd (transposeG g)))
