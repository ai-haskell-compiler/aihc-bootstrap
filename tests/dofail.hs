module Main where

-- A pattern on the left of <- that does not match is the monad's `fail`,
-- not an error. A pattern that always matches needs no MonadFail.

import Control.Monad.Trans.State.Strict (State, evalState, get, put)

data Shape = Circle Int | Square Int

newtype Box = Box Int

circles :: [Shape] -> [Int]
circles shapes = do
  Circle r <- shapes
  pure r

firstCircle :: Maybe Shape -> Maybe Int
firstCircle shape = do
  Circle r <- shape
  Just (r + 1)

pairs :: Maybe (Int, [Int]) -> Maybe Int
pairs value = do
  (a, b : _) <- value
  pure (a + b)

-- State has no MonadFail: these patterns cannot fail.
counter :: State Box (Int, Int)
counter = do
  Box n <- get
  put (Box (n + 1))
  (a, b) <- pure (n, n * 2)
  Box m <- get
  pure (a + m, b)

main :: IO ()
main = do
  print (circles [Circle 1, Square 2, Circle 3])
  print (firstCircle (Just (Circle 4)), firstCircle (Just (Square 4)), firstCircle Nothing)
  print (pairs (Just (1, [2, 3])), pairs (Just (1, [])))
  print (evalState counter (Box 5))
  [x, y] <- pure [1, 2 :: Int]
  print (x + y)
