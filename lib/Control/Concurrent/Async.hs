module Control.Concurrent.Async where

-- The interpreter has one thread, so nothing runs at the same time as
-- anything else:
--
--   * `async` does not start the action. The first `wait` runs it, and
--     keeps its result for the waits after that.
--   * An action that is never waited for never runs. That is what
--     happens to the action of `withAsync` when the body ignores it, as
--     for a ticker that redraws progress until it is cancelled.
--   * The functions that run a list of actions run them in order.

import Control.Exception (SomeException, throwIO, try)
import Data.IORef

-- The action, until it has run; then how it ended.
newtype Async a = Async (IORef (Either (IO a) (Either SomeException a)))

async :: IO a -> IO (Async a)
async action = fmap Async (newIORef (Left action))

withAsync :: IO a -> (Async a -> IO b) -> IO b
withAsync action body = async action >>= body

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async ref) = readIORef ref >>= \state -> case state of
  Right result -> pure result
  Left action -> do
    result <- try action
    writeIORef ref (Right result)
    pure result

wait :: Async a -> IO a
wait handle = waitCatch handle >>= either throwIO pure

poll :: Async a -> IO (Maybe (Either SomeException a))
poll (Async ref) = fmap (either (const Nothing) Just) (readIORef ref)

-- There is nothing running to stop.
cancel :: Async a -> IO ()
cancel _ = pure ()

concurrently :: IO a -> IO b -> IO (a, b)
concurrently first second = first >>= \a -> second >>= \b -> pure (a, b)

concurrently_ :: IO a -> IO b -> IO ()
concurrently_ first second = first >> second >> pure ()

mapConcurrently :: Traversable t => (a -> IO b) -> t a -> IO (t b)
mapConcurrently = mapM

forConcurrently :: Traversable t => t a -> (a -> IO b) -> IO (t b)
forConcurrently container action = mapM action container

mapConcurrently_ :: Foldable t => (a -> IO b) -> t a -> IO ()
mapConcurrently_ = mapM_

forConcurrently_ :: Foldable t => t a -> (a -> IO b) -> IO ()
forConcurrently_ container action = mapM_ action container

replicateConcurrently :: Int -> IO a -> IO [a]
replicateConcurrently count action = mapM (const action) [1 .. count]

replicateConcurrently_ :: Int -> IO a -> IO ()
replicateConcurrently_ count action = mapM_ (const action) [1 .. count]
