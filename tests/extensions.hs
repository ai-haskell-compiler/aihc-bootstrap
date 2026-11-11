{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Char (isDigit, toUpper)
import Data.List (sortOn, stripPrefix)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)

-- ---- LambdaCase ----

describe :: Maybe Int -> String
describe = \case
  Nothing -> "nothing"
  Just 0 -> "zero"
  Just n
    | n < 0 -> "negative"
    | otherwise -> "positive"

classify :: [Either String Int] -> [String]
classify = map $ \case
  Left s -> "text " ++ s
  Right n -> "number " ++ show n

-- ---- ViewPatterns ----

firstWord :: String -> String
firstWord (words -> w : _) = w
firstWord _ = "(none)"

command :: String -> String
command (stripPrefix "add " -> Just rest) = "adding " ++ rest
command (stripPrefix "remove " -> Just rest) = "removing " ++ rest
command (map toUpper -> "HELP") = "helping"
command other = "unknown: " ++ other

lookupBoth :: Map.Map String Int -> (String, String) -> Maybe Int
lookupBoth table (flip Map.lookup table -> Just a, flip Map.lookup table -> Just b) = Just (a + b)
lookupBoth _ _ = Nothing

total :: NonEmpty Int -> Int
total (NE.toList -> numbers) = sum numbers

nested :: [(Int, String)] -> [String]
nested pairs = [s ++ show n | (subtract 1 -> n, reverse -> s) <- pairs, n > 0]

viaCase :: String -> Int
viaCase text = case text of
  (span isDigit -> (digits@(_ : _), rest)) -> 100 * length digits + length rest
  (length -> n) -> negate n

-- ---- NamedFieldPuns, also on the left of <- ----

data Point = Point {px :: Int, py :: Int, label :: String}

norm :: Point -> Int
norm Point {px, py} = abs px + abs py

move :: Int -> Point -> Point
move dx point@Point {px} = point {px = px + dx}

points :: [Point] -> Maybe String
points list = do
  Point {label, py} <- firstAbove list
  Point {px} <- Just (move py (Point 1 2 label))
  pure (label ++ show px)
  where
    firstAbove candidates = case filter (\Point {py} -> py > 0) candidates of
      [] -> Nothing
      p : _ -> Just p

-- ---- TupleSections ----

tagged :: [(String, Int)]
tagged = map ("tag",) [1, 2, 3]

indexed :: [(Int, Char, Bool)]
indexed = zipWith (,'x',) [1, 2] [True, False]

paired :: Maybe (Int, String)
paired = (,"second") <$> Just 1

-- ---- Bang patterns in clauses, at the top and locally ----

sumStrict :: Int -> [Int] -> Int
sumStrict !acc [] = acc
sumStrict !acc (x : xs) = sumStrict (acc + x) xs

count :: (a -> Bool) -> [a] -> Int
count test = go 0
  where
    go !n [] = n
    go !n (a : rest) = go (if test a then n + 1 else n) rest

-- The operator ! is still an operator, and operators can be defined
-- between their parameters.
(!) :: [a] -> Int -> a
list ! index = list !! index

infixr 5 +++

(+++) :: [a] -> [a] -> [a]
[] +++ ys = ys
(x : xs) +++ ys = x : (xs +++ ys)

divides :: Int -> Int -> Bool
a `divides` b = b `mod` a == 0

Just a <+> Just b = Just (a + b)
_ <+> _ = Nothing

locally :: Int -> [Int]
locally n = [n |> 1, n `times` 3, 2 .- n]
  where
    a |> b = a * 10 + b
    a `times` b
      | b <= 0 = 0
      | otherwise = a + (a `times` (b - 1))
    x .- y = x - y

-- ---- let in guards ----

grade :: [Int] -> String
grade scores
  | null scores = "no scores"
  | let best = maximum scores,
    let worst = minimum scores,
    best - worst > 50 =
      "uneven: " ++ show (best - worst)
  | let average = sum scores `div` length scores, average >= 50 = "pass at " ++ show average
  | otherwise = "fail"

pick :: Int -> Maybe (Int, Int)
pick n = case n of
  _ | let (q, r) = n `divMod` 7, r /= 0, Just d <- lookup q table -> Just (d, r)
  _ -> Nothing
  where
    table = [(0, 10), (1, 20), (2, 30)]

-- ---- Operators that are constructors, with a qualifier ----

build :: Int -> [Int] -> NonEmpty Int
build = (NE.:|)

firstOf :: NonEmpty a -> a
firstOf (a NE.:| _) = a

main :: IO ()
main = do
  print (map describe [Nothing, Just 0, Just (-3), Just 3])
  print (classify [Left "a", Right 1])
  print (map (\case 'a' -> 1; 'b' -> 2; _ -> 0 :: Int) "abc")
  print (map firstWord ["hello world", "  ", "one"])
  print (map command ["add x", "remove y", "help", "other"])
  let table = Map.fromList [("a", 1), ("b", 2)]
  print (lookupBoth table ("a", "b"), lookupBoth table ("a", "z"))
  print (total (build 1 [2, 3]))
  print (nested [(1, "ab"), (3, "cd"), (5, "ef")])
  print (map viaCase ["123abc", "abc", "7"])
  print (norm (Point 3 (-4) "p"), px (move 5 (Point 1 2 "q")))
  print (points [Point 0 0 "a", Point 9 4 "b"], points [])
  print tagged
  print indexed
  print paired
  print (map (,) "ab" <*> [True])
  print (sumStrict 0 [1 .. 100], count even [1 .. 10 :: Int], "abc" ! 1)
  print ([1, 2] +++ [3] +++ [], 3 `divides` 9, 4 `divides` 9, Just 1 <+> Just 2, Just 1 <+> Nothing, locally 4)
  print (map grade [[], [10, 90], [60, 70], [20, 30]])
  print (map pick [1, 8, 14, 15, 22])
  print (firstOf (build 9 []), NE.toList (0 NE.:| [1]))
  print (sortOn negate (mapMaybe (\case (n, True) -> Just n; _ -> Nothing) (map (,True) [3, 1, 2 :: Int])))
