module Main where

import qualified Prelude as P
import Prelude (putStrLn, return, (>>))

infixr 9 .

infixr 0 $

(.) f g = \x -> f (g x)

($) f x = f x

data Colour = Red | Green | Blue

data Shape = Circle Colour | Square Colour Colour

newtype Wrapper = Wrapper Shape

name :: Colour -> P.String
name colour = case colour of
  Red -> "red"
  Green -> "green"
  Blue -> "blue"

outline :: Wrapper -> Colour
outline (Wrapper shape) = case shape of
  Circle colour -> colour
  Square _ colour -> colour

main :: P.IO ()
main = do
  putStrLn (name Green)
  line <- return "bound"
  putStrLn line
  () <- putStrLn . name $ outline (Wrapper (Square Red Blue))
  shape <- return (Circle Red :: Shape)
  case shape of
    Circle colour -> do
      putStrLn (name colour)
      putStrLn "nested"
    Square _ _ -> putStrLn "wrong"
  (\(Circle colour) -> putStrLn (name colour)) (Circle Green) >> putStrLn "done"
