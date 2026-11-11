module Control.Concurrent
  ( module Control.Concurrent,
    module Control.Concurrent.MVar,
  )
where

-- The interpreter has one thread, and says so: getNumCapabilities is 1.
-- See Control.Concurrent.MVar for what that means for waiting.

import Control.Concurrent.MVar

getNumCapabilities :: IO Int
getNumCapabilities = pure 1

setNumCapabilities :: Int -> IO ()
setNumCapabilities _ = pure ()

-- Waits for that many microseconds.
threadDelay :: Int -> IO ()
threadDelay = sleepMicroseconds

yield :: IO ()
yield = pure ()

foreign import prim "sleepMicroseconds" sleepMicroseconds :: Int -> IO ()
