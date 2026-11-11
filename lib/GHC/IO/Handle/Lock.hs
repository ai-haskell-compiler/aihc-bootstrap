module GHC.IO.Handle.Lock where

-- Locks on files keep two programs from changing the same thing at once.
-- The interpreter runs one program with one thread, and does not lock:
-- two runs at the same time must not share a build directory.

import System.IO (Handle)

data LockMode = SharedLock | ExclusiveLock

hLock :: Handle -> LockMode -> IO ()
hLock _ _ = pure ()

hTryLock :: Handle -> LockMode -> IO Bool
hTryLock _ _ = pure True

hUnlock :: Handle -> IO ()
hUnlock _ = pure ()
