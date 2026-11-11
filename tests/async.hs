module Main where

-- Needs the package async, which GHC does not ship. The expected output
-- was checked by hand, not generated with runghc as for most tests. Every
-- result below is the same with real threads; the order of the lines
-- written by actions that run "at the same time" is not, so those actions
-- write nothing.

import Control.Concurrent.Async
import Control.Exception
import Control.Monad (forever)
import Data.IORef

main :: IO ()
main = do
  a <- async (pure (6 * 7 :: Int))
  print =<< wait a
  print =<< wait a

  -- An action runs once, however often it is waited for.
  runs <- newIORef (0 :: Int)
  b <- async (modifyIORef runs (+ 1) >> pure "ran")
  first <- wait b
  second <- wait b
  count <- readIORef runs
  print (first, second, count)

  -- A failure arrives at the wait.
  c <- async (throwIO (ErrorCall "failed") :: IO Int)
  caught <- try (wait c)
  print (caught :: Either ErrorCall Int)
  viaCatch <- waitCatch c
  putStrLn (either (const "Left") show viaCatch)

  print =<< mapConcurrently (pure . (* 2)) [1, 2, 3 :: Int]
  print =<< forConcurrently "abc" (pure . succ)
  total <- newIORef (0 :: Int)
  mapConcurrently_ (\n -> atomicModifyIORef' total (\t -> (t + n, ()))) [1 .. 10]
  forConcurrently_ [1 .. 10] (\n -> atomicModifyIORef' total (\t -> (t + n, ())))
  print =<< readIORef total
  print =<< concurrently (pure 'x') (pure True)
  print =<< replicateConcurrently 3 (pure 'r')

  -- A ticker that runs until it is cancelled, around a body that ignores it.
  print =<< withAsync (forever (pure ())) (\_ -> pure "the body's result")
  print =<< withAsync (pure (5 :: Int)) wait
