{-# LANGUAGE RankNTypes #-}

module Main where

import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, modifySTRef, newSTRef, readSTRef)

-- An argument that has to work at every type.
both :: (forall a. [a] -> Int) -> ([Int], String) -> (Int, Int)
both measure (numbers, text) = (measure numbers, measure text)

applyTwice :: (forall a. a -> a) -> (Int, Bool) -> (Int, Bool)
applyTwice f (n, b) = (f n, f b)

-- With a class: the instance is chosen where the argument is used.
showBoth :: (forall a. Show a => a -> String) -> Int -> Bool -> [String]
showBoth render n b = [render n, render b]

-- Passed on to another function that wants the same.
passOn :: (forall a. [a] -> Int) -> (Int, Int)
passOn measure = both measure ([1, 2, 3], "ab")

-- An action in ST that works for every state thread, as AIHC's object
-- writer takes one.
withCounter :: (forall s. STRef s Int -> ST s ()) -> Int
withCounter action = runST (do
  counter <- newSTRef 0
  action counter
  action counter
  readSTRef counter)

-- Through a helper that is local and has a signature of its own.
collect :: (forall a. Show a => a -> String) -> [String]
collect render = go 3
  where
    go :: Int -> [String]
    go 0 = [render 'z']
    go n = render n : go (n - 1)

-- Two arguments of this kind.
choose :: Bool -> (forall a. a -> a) -> (forall a. a -> a) -> (Int, Char) -> (Int, Char)
choose flag first second (n, c) = if flag then (first n, first c) else (second n, second c)

newtype Wrapper = Wrapper Int
  deriving (Show)

main :: IO ()
main = do
  print (both length ([1, 2, 3], "four"))
  print (both (\list -> length list * 2) ([], "ab"))
  print (applyTwice id (1, True))
  print (showBoth show 7 False)
  print (showBoth (\a -> "<" ++ show a ++ ">") 7 False)
  print (passOn length, passOn (const 0))
  print (withCounter (\ref -> modifySTRef ref (+ 5)))
  print (collect show, collect (reverse . show))
  print (choose True id id (1, 'a'), choose False id (\x -> x) (2, 'b'))
