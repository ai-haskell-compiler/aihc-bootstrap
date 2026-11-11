module Main where

main :: IO ()
main = greet "Hello, world!"

greet :: String -> IO ()
greet message = (\text -> putStrLn text) message
