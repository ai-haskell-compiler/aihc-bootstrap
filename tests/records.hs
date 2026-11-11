{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

data Colour = Red | Green

name :: Colour -> String
name Red = "red"
name Green = "green"

data Shape
  = Circle {colour :: Colour, label :: String}
  | Square {colour :: Colour, side :: !String, label :: String}

data Pair a b = Pair {first :: a, second :: b}

-- Fields as functions, also for a field that several constructors have.
describe :: Shape -> String
describe shape = name (colour shape) ++ " " ++ label shape

-- Patterns: a field with a pattern, a field given by its name, and `..`.
kind :: Shape -> String
kind Circle {} = "circle"
kind Square {colour = Red} = "red square"
kind Square {side} = "square of side " ++ side

everything :: Shape -> String
everything Circle {..} = name colour ++ "/" ++ label
everything Square {..} = name colour ++ "/" ++ side ++ "/" ++ label

-- Construction: fields in any order, by name, and from variables in scope.
make :: String -> Shape
make label = Square {side = "2", label, colour = Green}

makeAll :: Colour -> Shape
makeAll colour = Circle {..}
  where
    label = "from scope"

-- Update, which may change the type.
swap :: Pair a b -> Pair b a
swap pair = pair {first = second pair, second = first pair}

-- Sections.
exclaim :: String -> String
exclaim = (++ "!")

greet :: String -> String
greet = ("hello " ++)

main :: IO ()
main = do
  let circle = Circle {label = "round", colour = Red}
      square = make "boxy"
  putStrLn (describe circle)
  putStrLn (describe square)
  putStrLn (kind circle)
  putStrLn (kind square)
  putStrLn (kind square {colour = Red})
  putStrLn (everything (makeAll Green))
  putStrLn (everything square {label = "renamed", side = "3"})
  putStrLn (first (swap (Pair {first = Red, second = "second"})))
  putStrLn (exclaim (greet "world"))
  putStrLn (name (second (swap (Pair Green "x"))))
  putStrLn (concatMap (`pick` Green) [Red, Green])
  where
    pick a _ = name a ++ ";"
