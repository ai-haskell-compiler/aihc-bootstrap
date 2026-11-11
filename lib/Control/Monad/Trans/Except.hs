module Control.Monad.Trans.Except where

-- A computation that can stop with an error of type e, over the monad m.

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor.Identity (Identity (..))

newtype ExceptT e m a = ExceptT (m (Either e a))

type Except e = ExceptT e Identity

runExceptT :: ExceptT e m a -> m (Either e a)
runExceptT (ExceptT action) = action

instance Functor m => Functor (ExceptT e m) where
  fmap f (ExceptT action) = ExceptT (fmap (fmap f) action)

instance Monad m => Applicative (ExceptT e m) where
  pure a = ExceptT (pure (Right a))
  (<*>) (ExceptT function) (ExceptT argument) = ExceptT (function >>= \found -> case found of
    Left e -> pure (Left e)
    Right f -> fmap (fmap f) argument)

instance Monad m => Monad (ExceptT e m) where
  (>>=) (ExceptT action) next = ExceptT (action >>= \found -> case found of
    Left e -> pure (Left e)
    Right a -> runExceptT (next a))

instance MonadFail m => MonadFail (ExceptT e m) where
  fail message = ExceptT (fail message)

instance MonadTrans (ExceptT e) where
  lift action = ExceptT (fmap Right action)

instance MonadIO m => MonadIO (ExceptT e m) where
  liftIO action = lift (liftIO action)

except :: Monad m => Either e a -> ExceptT e m a
except result = ExceptT (pure result)

throwE :: Monad m => e -> ExceptT e m a
throwE e = ExceptT (pure (Left e))

catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e2 m a) -> ExceptT e2 m a
catchE (ExceptT action) handler = ExceptT (action >>= \found -> case found of
  Left e -> runExceptT (handler e)
  Right a -> pure (Right a))

withExceptT :: Functor m => (e -> e2) -> ExceptT e m a -> ExceptT e2 m a
withExceptT f (ExceptT action) = ExceptT (fmap (either (Left . f) Right) action)

mapExceptT :: (m (Either e a) -> n (Either e2 b)) -> ExceptT e m a -> ExceptT e2 n b
mapExceptT f (ExceptT action) = ExceptT (f action)

runExcept :: Except e a -> Either e a
runExcept (ExceptT action) = runIdentity action
