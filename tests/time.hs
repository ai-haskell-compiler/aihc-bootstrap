module Main where

import Data.List (zipWith4)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (getModificationTime)
import System.Environment (getEnvironment, getExecutablePath)
main :: IO ()
main = do
  print (map (posixSecondsToUTCTime . fromInteger) [0, 951782400, 1709251199, 1794402900, -1, 4102444800])
  putStrLn (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (posixSecondsToUTCTime 1794402900))
  time <- getModificationTime "Makefile"
  print (time == time, length (show time) >= 23)
  env <- getEnvironment
  print (lookup "HOME" env /= Nothing, zipWith4 (\a b c d -> a + b + c + d) [1, 2] [10, 20] [100, 200] [1000, 2000 :: Int])
  path <- getExecutablePath
  print (take 1 path)
