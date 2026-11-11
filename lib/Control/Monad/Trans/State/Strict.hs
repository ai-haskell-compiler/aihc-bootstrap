module Control.Monad.Trans.State.Strict where

-- A computation with a state of type s, over the monad m. Strict: the pair
-- of result and state is evaluated at each step, as in the transformers
-- package.

import Control.Applicative (Alternative (..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Functor.Identity (Identity (..))

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

type State s = StateT s Identity

instance Functor m => Functor (StateT s m) where
  fmap f (StateT run) = StateT (\s -> fmap (\(a, next) -> (f a, next)) (run s))

instance Monad m => Applicative (StateT s m) where
  pure a = StateT (\s -> pure (a, s))
  (<*>) (StateT runF) (StateT runA) = StateT (\s ->
    runF s >>= \(f, s1) -> runA s1 >>= \(a, s2) -> pure (f a, s2))
  (*>) first second = first >>= \_ -> second

instance Monad m => Monad (StateT s m) where
  (>>=) (StateT run) next = StateT (\s -> run s >>= \(a, s1) -> runStateT (next a) s1)

instance MonadFail m => MonadFail (StateT s m) where
  fail message = StateT (\_ -> fail message)

instance (Monad m, Alternative m) => Alternative (StateT s m) where
  empty = StateT (\_ -> empty)
  (<|>) (StateT first) (StateT second) = StateT (\s -> first s <|> second s)

instance MonadTrans (StateT s) where
  lift action = StateT (\s -> action >>= \a -> pure (a, s))

instance MonadIO m => MonadIO (StateT s m) where
  liftIO action = lift (liftIO action)

state :: Monad m => (s -> (a, s)) -> StateT s m a
state f = StateT (\s -> pure (f s))

get :: Monad m => StateT s m s
get = state (\s -> (s, s))

put :: Monad m => s -> StateT s m ()
put s = state (\_ -> ((), s))

modify :: Monad m => (s -> s) -> StateT s m ()
modify f = state (\s -> ((), f s))

modify' :: Monad m => (s -> s) -> StateT s m ()
modify' f = get >>= \s -> put $! f s

gets :: Monad m => (s -> a) -> StateT s m a
gets f = state (\s -> (f s, s))

evalStateT :: Monad m => StateT s m a -> s -> m a
evalStateT (StateT run) s = run s >>= \(a, _) -> pure a

execStateT :: Monad m => StateT s m a -> s -> m s
execStateT (StateT run) s = run s >>= \(_, final) -> pure final

mapStateT :: (m (a, s) -> n (b, s)) -> StateT s m a -> StateT s n b
mapStateT f (StateT run) = StateT (\s -> f (run s))

withStateT :: (s -> s) -> StateT s m a -> StateT s m a
withStateT f (StateT run) = StateT (\s -> run (f s))

runState :: State s a -> s -> (a, s)
runState (StateT run) s = runIdentity (run s)

evalState :: State s a -> s -> a
evalState action s = fst (runState action s)

execState :: State s a -> s -> s
execState action s = snd (runState action s)

mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f = mapStateT (\(Identity pair) -> Identity (f pair))

withState :: (s -> s) -> State s a -> State s a
withState = withStateT
