module Data.Primitive.MutVar where

-- A mutable cell in ST.

import Control.Monad.ST (ST (..))
import Data.IORef

newtype MutVar s a = MutVar (IORef a)

newMutVar :: a -> ST s (MutVar s a)
newMutVar a = ST (fmap MutVar (newIORef a))

readMutVar :: MutVar s a -> ST s a
readMutVar (MutVar ref) = ST (readIORef ref)

writeMutVar :: MutVar s a -> a -> ST s ()
writeMutVar (MutVar ref) a = ST (writeIORef ref a)

modifyMutVar :: MutVar s a -> (a -> a) -> ST s ()
modifyMutVar (MutVar ref) f = ST (modifyIORef ref f)

modifyMutVar' :: MutVar s a -> (a -> a) -> ST s ()
modifyMutVar' (MutVar ref) f = ST (modifyIORef' ref f)
