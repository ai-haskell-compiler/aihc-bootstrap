{-# LANGUAGE DeriveGeneric #-}

module Main where

-- Every type can be evaluated completely. An instance that the program
-- gives for a type of its own is the one used for that type.

import Control.DeepSeq (NFData (..), deepseq, force)
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
