{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.String (IsString (..))

newtype Name = Name String

instance IsString Name where
  fromString = Name

instance Eq Name where
  (==) (Name a) (Name b) = a == b

-- A string pattern at a type that is not String compares with ==.
describe :: Name -> String
describe "special" = "the special name"
describe (Name text) = "name " ++ text

-- The same pattern at type String.
plain :: String -> String
plain "" = "empty"
plain "special" = "special string"
plain other = other

-- Number patterns compare with == too.
size :: Int -> String
size 0 = "zero"
size 1 = "one"
size _ = "many"

count :: [a] -> Int
count [] = 0
count [_] = 1
count _ = 2

main :: IO ()
main = do
  putStrLn (describe "special")
  putStrLn (describe "other")
  putStrLn (plain "")
  putStrLn (plain "special")
  putStrLn (plain "rest")
  putStrLn (size 0)
  putStrLn (size (count "x"))
  putStrLn (size (count "xyz"))
