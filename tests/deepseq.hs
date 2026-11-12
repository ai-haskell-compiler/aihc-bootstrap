{-# LANGUAGE DeriveGeneric #-}

module Main where

-- Every type can be evaluated completely. An instance that the program
-- gives for a type of its own is the one used for that type.

import Control.DeepSeq (NFData (..), deepseq, force)
import Control.Exception (ErrorCall, evaluate, try)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Generics (Generic)

data Lazy = Lazy Int

instance NFData Lazy where
  rnf _ = ()

newtype Wrap = Wrap [Int] deriving (Generic)

instance NFData Wrap

main :: IO ()
main = do
  print (rnf (1 :: Int, "two"), rnf [Just 'x'], rnf (Wrap [1, 2]))
  print (rnf (Lazy undefined))
  print (length (force [1, 2, 3 :: Int]))
  -- A value that is part of many others is only walked once.
  let shared = [1 .. 1000 :: Int]
      many = replicate 100000 shared
  print (many `deepseq` length many, sum (force (many !! 99999)))
  -- What is put into a reference later is evaluated when asked again.
  ref <- newIORef [1 :: Int]
  first <- readIORef ref
  print (rnf first)
  writeIORef ref [1, undefined]
  second <- readIORef ref
  result <- try (evaluate (rnf second))
  print (either (\e -> "failed: " ++ takeWhile (/= '\n') (show (e :: ErrorCall))) show result)
