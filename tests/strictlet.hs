{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (ErrorCall (..), evaluate, try)

-- A binding with a ! in front is evaluated before the body of its let or
-- where, even if nothing uses it. aihc-parser relies on that to keep a
-- position instead of the whole parser state.

strictness :: String -> a -> IO ()
strictness name value = do
  result <- try (evaluate value)
  putStrLn $ case result of
    Left (ErrorCall message) -> name ++ ": strict, " ++ message
    Right _ -> name ++ ": lazy"

inLet :: String
inLet = let !a = error "a" :: Int in "unused"

lazyNeighbour :: String
lazyNeighbour =
  let !b = 1 :: Int
      c = error "c" :: Int
   in "neighbour"

inWhere :: Int -> String
inWhere x = "unused"
  where
    !d = if x > 0 then error "d" else x

-- The bang only asks for the outside: the fields stay as they are.
tuple :: String
tuple = let !(e, f) = (error "e", error "f") :: (Int, Int) in "fields"

main :: IO ()
main = do
  strictness "let" inLet
  strictness "lazy neighbour" lazyNeighbour
  strictness "where" (inWhere 1)
  strictness "where, not taken" (inWhere 0)
  strictness "tuple" tuple
  r <- try $ do
    let !g = error "g" :: Int
    putStrLn "not printed"
  putStrLn $ case r of
    Left (ErrorCall message) -> "do: strict, " ++ message
    Right () -> "do: lazy"
  -- A thunk whose evaluation raised raises the same again.
  let h = error "h" :: Int
  first <- try (evaluate h)
  second <- try (evaluate h)
  print (either (\(ErrorCall m) -> m) show first, either (\(ErrorCall m) -> m) show second)
