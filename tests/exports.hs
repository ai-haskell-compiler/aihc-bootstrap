{-# LANGUAGE PatternSynonyms #-}

module Main where

-- What an export list lets through, and what it hides. The modules are in
-- tests/lib.

import Shapes (Shape (..), Size (..), describe, double, pattern Tiny)
import qualified Shapes as S

-- These names are defined in the imported modules too, but not exported
-- from them, so there is nothing ambiguous here.
helper :: Int
helper = 7

secret :: String
secret = "mine"

data Hidden = Hidden
  deriving (Show)

height :: Int
height = 99

name :: Shape -> String
name shape = case shape of
  Unit -> "unit"
  Tiny -> "tiny"
  Circle _ -> "circle"
  Square _ -> "square"
  _ -> "something else"

main :: IO ()
main = do
  print (map name [Unit, Tiny, Circle 2, Square 1, Square 3, double Tiny])
  print (map S.area [Unit, double (Square 2), S.Tiny])
  putStrLn (describe (Circle 1))
  print (helper, secret, Hidden, height)
  let size = Size 3 4
  print (width size, case size of Size w h -> w * h)
  print (case size of Size {width = w} -> w + 1)
