module Main where

import qualified Data.Map as Map

-- Local functions without type signatures that call each other. They are
-- generalised together, so each can be used at several types afterwards.

parity :: Int -> (Bool, Bool)
parity n = (isEven n, isOdd n)
  where
    isEven k = k == 0 || isOdd (k - 1)
    isOdd k = k /= 0 && isEven (k - 1)

-- Polymorphic: used at Int and at Char below.
alternate :: ([Int], String)
alternate = (firsts [1, 2, 3, 4, 5], firsts "abcdef")
  where
    firsts list = case list of
      [] -> []
      x : rest -> x : seconds rest
    seconds list = case list of
      [] -> []
      _ : rest -> firsts rest

-- A helper of a local function that calls the function it is local to.
flatten :: [[Int]] -> [String]
flatten lists = map show (walk lists) ++ [walk ["ab", "cd"]]
  where
    walk nested = case nested of
      [] -> []
      inner : rest -> each inner rest
      where
        each items rest = case items of
          [] -> walk rest
          item : more -> item : each more rest

-- Three functions in a ring, with a class used at the group's type.
ring :: (String, String)
ring = (a 5 [1 :: Int], a 3 "x")
  where
    a n xs = if n <= 0 then show xs else b (n - 1) (xs ++ xs)
    b n xs = c n (take 3 xs)
    c n xs = a n (reverse xs)

-- A group reached through a binding that is not a function.
table :: Map.Map Int Int
table = Map.fromList pairs
  where
    pairs = [(n, collatz n) | n <- [1 .. 10]]
    collatz n = if n == 1 then 0 else 1 + step n
    step n = if even n then collatz (n `div` 2) else collatz (3 * n + 1)

-- A function that calls itself through two helpers defined after it, one
-- of which also calls itself.
render :: Int -> String
render = top
  where
    top n
      | n < 10 = digit n
      | otherwise = top (n `div` 10) ++ sep n ++ digit (n `mod` 10)
    sep n = if n > 1000 then "," ++ pad 0 else ""
    pad k = if k >= 1 then "" else "" ++ pad (k + 1)
    digit d = [toEnum (fromEnum '0' + d)]

-- The group is inside a let, with a where around it.
total :: [Int] -> Int
total numbers =
  let go acc list = case list of
        [] -> finish acc
        x : rest -> more (acc + x) rest
      more acc rest = go (acc * scale) rest
   in go 0 numbers
  where
    scale = 1
    finish = id

main :: IO ()
main = do
  print (map parity [0, 1, 2, 7])
  print alternate
  print (flatten [[1, 2], [], [3]])
  print ring
  print (Map.toList table)
  print (map render [7, 42, 12345])
  print (total [1, 2, 3, 4])
