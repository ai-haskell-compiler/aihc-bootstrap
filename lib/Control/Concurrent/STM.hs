module Control.Concurrent.STM where

-- The interpreter has one thread, so a transaction is an IO action that
-- nothing can interrupt. `retry` waits for another thread to change
-- something, which cannot happen: it ends the transaction with the
-- exception GHC throws for a transaction that can never continue.
--
-- One difference from GHC: a transaction that ends with an exception, or
-- the first half of an `orElse` that retries, keeps what it wrote.

import Control.Exception (BlockedIndefinitelyOnSTM (..), Exception, catch, throwIO)
import Data.IORef

newtype STM a = STM (IO a)

instance Functor STM where
  fmap f (STM action) = STM (fmap f action)

instance Applicative STM where
  pure a = STM (pure a)
  (<*>) (STM f) (STM a) = STM (f <*> a)

instance Monad STM where
  (>>=) (STM action) f = STM (action >>= \a -> case f a of STM next -> next)

-- Thrown by retry, caught by orElse and atomically.
data Retry = Retry

instance Show Retry where
  show _ = "retry"

instance Exception Retry

atomically :: STM a -> IO a
atomically (STM action) = action `catch` (\Retry -> throwIO BlockedIndefinitelyOnSTM)

retry :: STM a
retry = STM (throwIO Retry)

check :: Bool -> STM ()
check condition = if condition then pure () else retry

-- The second transaction, if the first one retries.
orElse :: STM a -> STM a -> STM a
orElse (STM first) (STM second) = STM (first `catch` (\Retry -> second))

throwSTM :: Exception e => e -> STM a
throwSTM e = STM (throwIO e)

-- ---- TVar ----

newtype TVar a = TVar (IORef a)

newTVar :: a -> STM (TVar a)
newTVar a = STM (newTVarIO a)

newTVarIO :: a -> IO (TVar a)
newTVarIO a = fmap TVar (newIORef a)

readTVar :: TVar a -> STM a
readTVar var = STM (readTVarIO var)

readTVarIO :: TVar a -> IO a
readTVarIO (TVar ref) = readIORef ref

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar ref) a = STM (writeIORef ref a)

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar (TVar ref) f = STM (modifyIORef ref f)

modifyTVar' :: TVar a -> (a -> a) -> STM ()
modifyTVar' (TVar ref) f = STM (modifyIORef' ref f)

stateTVar :: TVar s -> (s -> (a, s)) -> STM a
stateTVar var f = readTVar var >>= \s -> case f s of
  (a, new) -> writeTVar var new >> pure a

swapTVar :: TVar a -> a -> STM a
swapTVar var new = readTVar var >>= \old -> writeTVar var new >> pure old

-- ---- TMVar: a TVar that is full or empty ----

newtype TMVar a = TMVar (TVar (Maybe a))

newTMVar :: a -> STM (TMVar a)
newTMVar a = fmap TMVar (newTVar (Just a))

newEmptyTMVar :: STM (TMVar a)
newEmptyTMVar = fmap TMVar (newTVar Nothing)

newTMVarIO :: a -> IO (TMVar a)
newTMVarIO a = fmap TMVar (newTVarIO (Just a))

newEmptyTMVarIO :: IO (TMVar a)
newEmptyTMVarIO = fmap TMVar (newTVarIO Nothing)

tryTakeTMVar :: TMVar a -> STM (Maybe a)
tryTakeTMVar (TMVar var) = swapTVar var Nothing

tryReadTMVar :: TMVar a -> STM (Maybe a)
tryReadTMVar (TMVar var) = readTVar var

-- False if it is full already.
tryPutTMVar :: TMVar a -> a -> STM Bool
tryPutTMVar (TMVar var) a = readTVar var >>= \contents -> case contents of
  Nothing -> writeTVar var (Just a) >> pure True
  Just _ -> pure False

isEmptyTMVar :: TMVar a -> STM Bool
isEmptyTMVar (TMVar var) = fmap (maybe True (const False)) (readTVar var)

takeTMVar :: TMVar a -> STM a
takeTMVar var = tryTakeTMVar var >>= maybe retry pure

readTMVar :: TMVar a -> STM a
readTMVar var = tryReadTMVar var >>= maybe retry pure

putTMVar :: TMVar a -> a -> STM ()
putTMVar var a = tryPutTMVar var a >>= check

swapTMVar :: TMVar a -> a -> STM a
swapTMVar var new = takeTMVar var >>= \old -> putTMVar var new >> pure old

-- Fills it, whatever it held.
writeTMVar :: TMVar a -> a -> STM ()
writeTMVar (TMVar var) a = writeTVar var (Just a)
