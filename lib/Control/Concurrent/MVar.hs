module Control.Concurrent.MVar where

-- The interpreter has one thread. So an MVar is a cell that is full or
-- empty, and an operation that would wait for another thread can never
-- continue: it throws the exception GHC throws for a thread that waits for
-- something no thread can give it.

import Control.Exception (BlockedIndefinitelyOnMVar (..), onException, throwIO)
import Data.IORef

newtype MVar a = MVar (IORef (Maybe a))

newMVar :: a -> IO (MVar a)
newMVar a = fmap MVar (newIORef (Just a))

newEmptyMVar :: IO (MVar a)
newEmptyMVar = fmap MVar (newIORef Nothing)

tryTakeMVar :: MVar a -> IO (Maybe a)
tryTakeMVar (MVar ref) = readIORef ref >>= \contents -> writeIORef ref Nothing >> pure contents

tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar (MVar ref) = readIORef ref

-- False if it is full already.
tryPutMVar :: MVar a -> a -> IO Bool
tryPutMVar (MVar ref) a = readIORef ref >>= \contents -> case contents of
  Nothing -> writeIORef ref (Just a) >> pure True
  Just _ -> pure False

isEmptyMVar :: MVar a -> IO Bool
isEmptyMVar (MVar ref) = fmap (maybe True (const False)) (readIORef ref)

takeMVar :: MVar a -> IO a
takeMVar var = tryTakeMVar var >>= maybe (throwIO BlockedIndefinitelyOnMVar) pure

readMVar :: MVar a -> IO a
readMVar var = tryReadMVar var >>= maybe (throwIO BlockedIndefinitelyOnMVar) pure

putMVar :: MVar a -> a -> IO ()
putMVar var a = tryPutMVar var a >>= \ok ->
  if ok then pure () else throwIO BlockedIndefinitelyOnMVar

swapMVar :: MVar a -> a -> IO a
swapMVar var new = takeMVar var >>= \old -> putMVar var new >> pure old

-- The MVar is empty while the action runs, and gets its old contents back
-- if the action fails.
withMVar :: MVar a -> (a -> IO b) -> IO b
withMVar var action = do
  a <- takeMVar var
  b <- action a `onException` putMVar var a
  putMVar var a
  pure b

modifyMVar_ :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_ var action = do
  a <- takeMVar var
  new <- action a `onException` putMVar var a
  putMVar var new

modifyMVar :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar var action = do
  a <- takeMVar var
  (new, b) <- action a `onException` putMVar var a
  putMVar var new
  pure b
