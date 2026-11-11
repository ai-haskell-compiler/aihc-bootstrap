module Data.STRef where

import Control.Monad.ST (ST (..))
import Data.IORef

newtype STRef s a = STRef (IORef a)

newSTRef :: a -> ST s (STRef s a)
newSTRef a = ST (fmap STRef (newIORef a))

readSTRef :: STRef s a -> ST s a
readSTRef (STRef ref) = ST (readIORef ref)

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef (STRef ref) a = ST (writeIORef ref a)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef (STRef ref) f = ST (modifyIORef ref f)

modifySTRef' :: STRef s a -> (a -> a) -> ST s ()
modifySTRef' (STRef ref) f = ST (modifyIORef' ref f)
