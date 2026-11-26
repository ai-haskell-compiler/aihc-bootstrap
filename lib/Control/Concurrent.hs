module Control.Concurrent
  ( module Control.Concurrent,
    module Control.Concurrent.MVar,
  )
where

-- The interpreter has one thread, and says so: getNumCapabilities is 1.
-- See Control.Concurrent.MVar for what that means for waiting.

import Control.Concurrent.MVar
import Control.Exception (Exception, throwIO)

-- The one thread there is.
data ThreadId = ThreadId Int
  deriving (Eq, Ord, Show)

myThreadId :: IO ThreadId
myThreadId = pure (ThreadId 0)

-- With one thread, the exception can only be for the thread that throws it.
throwTo :: Exception e => ThreadId -> e -> IO ()
throwTo _ = throwIO

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
