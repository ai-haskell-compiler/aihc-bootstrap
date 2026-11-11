module Control.Monad.Trans.Reader where

-- A computation that can read a value of type r, over the monad m.

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor.Identity (Identity (..))

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

type Reader r = ReaderT r Identity

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT run) = ReaderT (\r -> fmap f (run r))

instance Applicative m => Applicative (ReaderT r m) where
  pure a = ReaderT (\_ -> pure a)
  (<*>) (ReaderT runF) (ReaderT runA) = ReaderT (\r -> runF r <*> runA r)

instance Monad m => Monad (ReaderT r m) where
  (>>=) (ReaderT run) next = ReaderT (\r -> run r >>= \a -> runReaderT (next a) r)

instance MonadFail m => MonadFail (ReaderT r m) where
  fail message = ReaderT (\_ -> fail message)

instance Alternative m => Alternative (ReaderT r m) where
  empty = ReaderT (\_ -> empty)
  (<|>) (ReaderT first) (ReaderT second) = ReaderT (\r -> first r <|> second r)

instance MonadTrans (ReaderT r) where
  lift action = ReaderT (\_ -> action)

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO action = lift (liftIO action)

reader :: Monad m => (r -> a) -> ReaderT r m a
reader f = ReaderT (\r -> pure (f r))

ask :: Monad m => ReaderT r m r
ask = ReaderT pure

asks :: Monad m => (r -> a) -> ReaderT r m a
asks = reader

local :: (r -> r) -> ReaderT r m a -> ReaderT r m a
local f (ReaderT run) = ReaderT (\r -> run (f r))

withReaderT :: (r2 -> r) -> ReaderT r m a -> ReaderT r2 m a
withReaderT f (ReaderT run) = ReaderT (\r -> run (f r))

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f (ReaderT run) = ReaderT (\r -> f (run r))

runReader :: Reader r a -> r -> a
runReader (ReaderT run) r = runIdentity (run r)
