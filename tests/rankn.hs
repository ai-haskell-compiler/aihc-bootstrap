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

-- A field that works at every type: used as a function, taken out with a
-- pattern, and kept by a record update.
data Tools a = Tools
  { name :: String,
    each :: !(forall b. [b] -> a -> [(a, b)]),
    twice :: forall c. c -> (c, c)
  }

tools :: Tools Int
tools = Tools {name = "t", each = \list a -> map (\b -> (a, b)) list, twice = \c -> (c, c)}

use :: Tools a -> a -> ([(a, Char)], [(a, Bool)])
use t a = (each t "xy" a, each t [True] a)

viaPattern :: Tools a -> (Int, String) -> ((Int, Int), (String, String))
viaPattern (Tools _ _ dup) (n, s) = (dup n, dup s)

viaRecord :: Tools a -> a -> [(a, Int)]
viaRecord Tools {each = f} a = f [1, 2] a

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
  print (use tools 1)
  print (twice tools 'q', twice tools "s", viaPattern tools (3, "x"), viaRecord tools 0)
  print (name tools {name = "renamed"}, each (tools {twice = \c -> (c, c)}) "z" 5)
