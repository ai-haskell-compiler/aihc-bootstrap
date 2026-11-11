module Data.IORef where

-- A mutable cell. The interpreter runs one thing at a time, so the atomic
-- functions are the plain ones.

data IORef a

foreign import prim "newRef" newIORef :: a -> IO (IORef a)

foreign import prim "readRef" readIORef :: IORef a -> IO a

foreign import prim "writeRef" writeIORef :: IORef a -> a -> IO ()

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= \a -> writeIORef ref (f a)

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = readIORef ref >>= \a -> writeIORef ref $! f a

atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef ref f = readIORef ref >>= \a -> case f a of
  (new, result) -> writeIORef ref new >> pure result

atomicModifyIORef' :: IORef a -> (a -> (a, b)) -> IO b
atomicModifyIORef' ref f = readIORef ref >>= \a -> case f a of
  (new, result) -> seq new (seq result (writeIORef ref new >> pure result))

atomicWriteIORef :: IORef a -> a -> IO ()
atomicWriteIORef = writeIORef
