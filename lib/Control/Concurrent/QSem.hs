module Control.Concurrent.QSem where

-- A count of free places. With one thread, nothing can free a place while
-- waitQSem waits, so it fails like an MVar that would wait.

import Control.Exception (BlockedIndefinitelyOnMVar (..), throwIO)
import Data.IORef

newtype QSem = QSem (IORef Int)

newQSem :: Int -> IO QSem
newQSem places =
  if places < 0
    then ioError (userError "newQSem: Initial quantity must be non-negative")
    else fmap QSem (newIORef places)

waitQSem :: QSem -> IO ()
waitQSem (QSem ref) = readIORef ref >>= \places ->
  if places > 0 then writeIORef ref (places - 1) else throwIO BlockedIndefinitelyOnMVar

signalQSem :: QSem -> IO ()
signalQSem (QSem ref) = modifyIORef' ref (+ 1)
