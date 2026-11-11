module Control.Monad.Trans.Maybe where

-- A computation that can stop without a result, over the monad m.

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT action) = MaybeT (fmap (fmap f) action)

instance Monad m => Applicative (MaybeT m) where
  pure a = MaybeT (pure (Just a))
  (<*>) (MaybeT function) (MaybeT argument) = MaybeT (function >>= \found -> case found of
    Nothing -> pure Nothing
    Just f -> fmap (fmap f) argument)

instance Monad m => Monad (MaybeT m) where
  (>>=) (MaybeT action) next = MaybeT (action >>= \found -> case found of
    Nothing -> pure Nothing
    Just a -> runMaybeT (next a))

instance Monad m => MonadFail (MaybeT m) where
  fail _ = MaybeT (pure Nothing)

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT (pure Nothing)
  (<|>) (MaybeT first) (MaybeT second) = MaybeT (first >>= \found -> case found of
    Nothing -> second
    Just a -> pure (Just a))

instance MonadTrans MaybeT where
  lift action = MaybeT (fmap Just action)

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO action = lift (liftIO action)

hoistMaybe :: Applicative m => Maybe a -> MaybeT m a
hoistMaybe found = MaybeT (pure found)
