module Main where

-- Needs the package monoidmap, which GHC does not ship. The expected output
-- was therefore checked by hand against the package's documentation, not
-- generated with runghc as for the other tests.

import qualified Data.Map as Map
import Data.Monoid (Sum (..))
import Data.MonoidMap (MonoidMap)
import qualified Data.MonoidMap as MonoidMap
import Data.Set (Set)
import qualified Data.Set as Set

type Counts = MonoidMap Char (Sum Int)

count :: String -> Counts
count string = MonoidMap.fromList [(c, Sum 1) | c <- string]

shown :: Counts -> String
shown counts = concat [c : show n | (c, Sum n) <- MonoidMap.toList counts]

sets :: MonoidMap Int (Set Char) -> [(Int, String)]
sets m = [(k, Set.toList v) | (k, v) <- MonoidMap.toList m]

main :: IO ()
main = do
  let banana = count "banana"
  putStrLn (shown banana)
  print (getSum (MonoidMap.get 'a' banana), getSum (MonoidMap.get 'z' banana))
  print (MonoidMap.nullKey 'z' banana, MonoidMap.nonNullKey 'b' banana, MonoidMap.nonNullCount banana)
  print (Set.toList (MonoidMap.nonNullKeys banana))

  -- A value that becomes mempty is no longer stored.
  putStrLn (shown (MonoidMap.set 'b' 0 banana))
  putStrLn (shown (MonoidMap.set 'x' 0 banana))
  putStrLn (shown (MonoidMap.set 'x' 7 banana))
  putStrLn (shown (MonoidMap.adjust (+ 1) 'q' banana))
  putStrLn (shown (MonoidMap.adjust (subtract 1) 'b' banana))
  putStrLn (shown (MonoidMap.nullify 'a' banana))
  putStrLn (shown (MonoidMap.nullifyKeysNotIn (Set.fromList "abz") banana))
  print (MonoidMap.adjust (subtract 1) 'b' banana == count "anana")
  print (MonoidMap.null (MonoidMap.empty :: Counts), MonoidMap.null banana)

  putStrLn (shown (banana <> count "bandana"))
  putStrLn (shown (banana `MonoidMap.minus` count "nab"))
  putStrLn (shown (count "nab" `MonoidMap.minus` banana))
  putStrLn (shown (MonoidMap.unionWith max banana (count "bbbz")))
  putStrLn (shown (MonoidMap.unionWith min banana (count "bbbz")))
  putStrLn (shown (mempty <> banana <> mempty))

  putStrLn (shown (MonoidMap.filter (> 1) banana))
  putStrLn (shown (MonoidMap.filterWithKey (\k _ -> k /= 'a') banana))
  putStrLn (shown (MonoidMap.map (subtract 2) banana))
  putStrLn (shown (MonoidMap.mapWithKey (\k v -> if k == 'n' then 0 else v * 10) banana))

  putStrLn (shown (MonoidMap.singleton 'k' 3))
  putStrLn (shown (MonoidMap.singleton 'k' 0))
  putStrLn (shown (MonoidMap.fromListWith (\_ new -> new) [('a', 1), ('b', 2), ('a', 0)]))
  putStrLn (shown (MonoidMap.fromMap (Map.fromList [('a', 0), ('b', 2)])))
  putStrLn (shown (MonoidMap.fromMapWith (Sum . length) (Map.fromList [('a', ""), ('b', "xy")])))
  putStrLn (shown (MonoidMap.fromSet (\c -> if c == 'b' then 0 else 4) (Set.fromList "abc")))

  let users = MonoidMap.fromList [(1, Set.singleton 'x'), (2, Set.singleton 'y'), (1, Set.singleton 'w')]
  print (sets users)
  print (sets (MonoidMap.adjust (Set.delete 'y') 2 users))
  print (sets (MonoidMap.adjust (Set.insert 'n') 3 users))
  print (sets (MonoidMap.map (`Set.intersection` Set.fromList "wy") users))
  print (Set.toList (MonoidMap.get 9 users))

  let nested = MonoidMap.fromList [(1 :: Int, count "aab"), (2, count "c"), (1, count "b")]
  print [(k, shown v) | (k, v) <- MonoidMap.toList nested]
  print [(k, shown v) | (k, v) <- MonoidMap.toList (nested `MonoidMap.minus` MonoidMap.singleton 2 (count "c"))]
  putStrLn (shown (MonoidMap.get 1 nested `MonoidMap.minus` MonoidMap.get 2 nested))
