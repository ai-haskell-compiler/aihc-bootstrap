module Main where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (foldl', sort)
import qualified Data.Map as LazyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- Numbers in an order that is neither ascending nor descending.
scrambled :: [Int]
scrambled = [(n * 7919) `mod` 1009 | n <- [1 .. 400]]

big :: Map.Map Int Int
big = Map.fromList [(n, n * 2) | n <- scrambled]

main :: IO ()
main = do
  let m = Map.fromList [(3, "three"), (1, "one"), (2, "two"), (1, "uno")]
  print m
  print (Map.toList m, Map.keys m, Map.elems m, Map.size m, Map.null m, Map.null (Map.empty :: Map.Map Int Int))
  print (Map.lookup 1 m, Map.lookup 9 m, Map.member 2 m, Map.notMember 2 m, Map.findWithDefault "none" 7 m, m Map.! 3)
  print (Map.insert 0 "zero" m, Map.delete 2 m, Map.delete 9 m)
  print (Map.insertWith (++) 1 "new-" m, Map.insertWith (++) 5 "new-" m, Map.adjust (++ "!") 2 m)
  print (Map.fromListWith (++) [(1, "a"), (2, "b"), (1, "c")], Map.fromListWithKey (\k a b -> show k ++ a ++ b) [(1, "a"), (1, "b")])
  print (Map.union m (Map.fromList [(3, "THREE"), (4, "four")]), Map.unionWith (++) m (Map.fromList [(3, "+"), (4, "four")]))
  -- The left map wins, and its value comes first, whichever map is larger.
  let small = Map.fromList [(2, "s2"), (50, "s50")]
      large = Map.fromList [(k, "l" ++ show k) | k <- [1 .. 6 :: Int]]
  print (Map.union small large, Map.union large small)
  print (Map.unionWith (++) small large, Map.unionWith (++) large small, Map.unions [small, large, small])
  print (Map.unions [m, Map.singleton 9 "nine", Map.singleton 1 "ignored"], Map.unionsWith (++) [m, Map.singleton 1 "+"])
  print (Map.difference m (Map.fromList [(1, ()), (7, ())]), Map.intersection m (Map.fromList [(1, ()), (7, ())]))
  print (Map.intersectionWith (,) m (Map.fromList [(2, 'x'), (3, 'y'), (8, 'z')]))
  print (Map.filter ((> 3) . length) m, Map.filterWithKey (\k _ -> odd k) m, Map.partitionWithKey (\k _ -> k < 2) m)
  print (Map.map length m, Map.mapWithKey (\k v -> show k ++ v) m, Map.mapMaybe (\v -> if v == "two" then Nothing else Just (length v)) m)
  print (Map.mapKeys (`div` 2) m, Map.foldrWithKey (\k v rest -> show k ++ v ++ rest) "" m, sum (Map.map length m))
  print (Map.keysSet m, Map.restrictKeys m (Set.fromList [1, 3, 5]), Map.withoutKeys m (Set.fromList [1, 3, 5]))
  print (Map.fromSet (* 2) (Set.fromList [3, 1, 2 :: Int]), Map.toAscList (Map.fromDistinctAscList [(1 :: Int, 'a'), (2, 'b'), (5, 'c')]))
  print (Map.lookupLE 2 m, Map.lookupLE 0 m, Map.lookupMin m, Map.lookupMax m, Map.insertLookupWithKey (\_ a b -> a ++ b) 1 "x" m)
  print (m == Map.fromList (reverse (Map.toList m)), compare m (Map.delete 3 m), fmap length m, Map.toDescList m)
  print (Map.size big, sum (Map.elems big), take 5 (Map.toList big), Map.keys big == sort (Map.keys big))
  print (Map.size (foldl' (flip Map.delete) big (take 300 scrambled)), Map.lookup 500 big, Map.lookup 501 big)
  mapped <- Map.traverseWithKey (\k v -> print (k, v) >> pure (k + length v)) m
  print (mapped, Map.differenceWith (\a b -> if b then Just (a ++ "!") else Nothing) m (Map.fromList [(1, True), (2, False)]))
  -- The lazy map keeps values unevaluated.
  print (LazyMap.size (LazyMap.fromList [(1 :: Int, undefined :: Int), (2, 3)]), LazyMap.lookup 2 (LazyMap.insert 1 undefined (LazyMap.singleton (2 :: Int) 'k')))
  let s = Set.fromList "mississippi"
  print (s, Set.toList s, Set.size s, Set.member 'm' s, Set.notMember 'm' s, Set.null s)
  print (Set.insert 'a' s, Set.delete 'p' s, Set.union s (Set.fromList "xyz"), Set.unions [s, Set.singleton 'q'])
  print (Set.difference s (Set.fromList "ms"), s Set.\\ Set.fromList "i", Set.intersection s (Set.fromList "sunny"))
  print (Set.filter (> 'm') s, Set.map succ s, Set.disjoint s (Set.fromList "abc"), Set.isSubsetOf (Set.fromList "sip") s)
  print (Set.minView s, Set.findMax s, Set.lookupGT 'm' s, Set.lookupGT 's' s, Set.foldl' (flip (:)) [] s, Set.toAscList s)
  print (s == Set.fromList "imps", compare s (Set.fromList "imp"), sum (Set.fromList [3, 1, 3 :: Int]), maximum s)
  let im = IntMap.fromListWith (+) [(5, 1), (-3, 2), (5, 10)]
  print (im, IntMap.lookup 5 im, IntMap.findWithDefault 0 4 im, im IntMap.! (-3), IntMap.member 5 im, IntMap.toList (IntMap.map (* 2) im))
  print (IntMap.insert 1 0 im, IntMap.delete 5 im, IntMap.filter even im, IntMap.elems im, IntMap.mapWithKey (+) im)
  let is = IntSet.fromList [5, -1, 3, 5]
  print (is, IntSet.member 3 is, IntSet.toList is, IntSet.toDescList is, IntSet.size is, IntSet.union is (IntSet.singleton 9))
  print (IntSet.difference is (IntSet.fromList [5]), IntSet.intersection is (IntSet.fromList [5, 7]), IntSet.filter (> 0) is, IntSet.delete 3 is)
