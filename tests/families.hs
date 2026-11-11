{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Map as Map

-- A container that knows the type of its elements and of its keys.
class Container c where
  type Element c
  type Key c
  toPairs :: c -> [(Key c, Element c)]
  fromElements :: [Element c] -> c
  first :: c -> Maybe (Element c)

instance Container [a] where
  type Element [a] = a
  type Key [a] = Int
  toPairs = zip [0 ..]
  fromElements = id
  first list = case list of
    [] -> Nothing
    a : _ -> Just a

newtype Words = Words (Map.Map String Int)

instance Container Words where
  type Element Words = String
  type Key Words = String
  toPairs (Words m) = [(k, k) | (k, _) <- Map.toList m]
  fromElements strings = Words (Map.fromList (zip strings [1 ..]))
  first (Words m) = fmap fst (Map.lookupMin m)

data Pair = Pair Bool Char

instance Container Pair where
  type Element Pair = Either Bool Char
  type Key Pair = Bool
  toPairs (Pair b c) = [(False, Left b), (True, Right c)]
  fromElements _ = Pair False ' '
  first (Pair b _) = Just (Left b)

-- Class methods at the associated types: which instance of Show, Eq and
-- Ord is used is only known once the container is.
describe :: (Container c, Show (Key c), Show (Element c)) => c -> String
describe c = unwords [show k ++ "=" ++ show e | (k, e) <- toPairs c]

largest :: (Container c, Ord (Element c)) => c -> Maybe (Element c)
largest c = case map snd (toPairs c) of
  [] -> Nothing
  elements -> Just (maximum elements)

count :: (Container c, Eq (Element c)) => Element c -> c -> Int
count wanted c = length [() | (_, e) <- toPairs c, e == wanted]

-- A value whose type is an associated type of a type parameter.
data Found c = Found (Element c) [Key c]

findAll :: (Container c, Eq (Element c)) => Element c -> c -> Found c
findAll wanted c = Found wanted [k | (k, e) <- toPairs c, e == wanted]

showFound :: (Show (Element c), Show (Key c)) => Found c -> String
showFound (Found e keys) = show e ++ " at " ++ show keys

main :: IO ()
main = do
  putStrLn (describe "abc")
  putStrLn (describe [True, False])
  putStrLn (describe (fromElements ["one", "two"] :: Words))
  putStrLn (describe (Pair True 'x'))
  print (largest [3, 1, 4, 1, 5 :: Int])
  print (largest (fromElements (words "pear apple quince") :: Words))
  print (largest (Pair True 'q'))
  print (largest ([] :: [Int]))
  print (count 'a' "banana", count (Right 'x') (Pair True 'x'), count "b" (fromElements ["a", "b"] :: Words))
  putStrLn (showFound (findAll 'a' "banana"))
  putStrLn (showFound (findAll (Left True) (Pair True 'x')))
  print (first "xyz", first (fromElements ["m", "c"] :: Words))
  print (fmap (+ 1) (first [10, 20 :: Int]))
