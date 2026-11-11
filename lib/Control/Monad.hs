module Control.Monad (module Control.Monad, module Prelude) where

import Control.Applicative (Alternative (..))

infixr 1 >=>, <=<

infixl 4 <$!>

when :: Applicative f => Bool -> f () -> f ()
when condition action = if condition then action else pure ()

unless :: Applicative f => Bool -> f () -> f ()
unless condition action = if condition then pure () else action

forM :: Monad m => [a] -> (a -> m b) -> m [b]
forM list f = mapM f list

forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
forM_ container f = mapM_ f container

forever :: Monad m => m a -> m b
forever action = action >> forever action

void :: Functor f => f a -> f ()
void = fmap (\_ -> ())

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM keep (a : rest) =
  keep a >>= (\yes -> filterM keep rest >>= (\others -> pure (if yes then a : others else others)))

foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldM f start container = foldr (\a continue total -> f total a >>= continue) pure container start

foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
foldM_ f start container = foldM f start container >> pure ()

replicateM :: Applicative m => Int -> m a -> m [a]
replicateM n action = if n <= 0 then pure [] else liftA2 (:) action (replicateM (n - 1) action)

replicateM_ :: Applicative m => Int -> m a -> m ()
replicateM_ n action = if n <= 0 then pure () else action *> replicateM_ (n - 1) action

zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f as bs = sequenceA (zipWith f as bs)

zipWithM_ :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m ()
zipWithM_ f as bs = traverse_ id (zipWith f as bs)

mapAndUnzipM :: Applicative m => (a -> m (b, c)) -> [a] -> m ([b], [c])
mapAndUnzipM f list = fmap unzip (traverse f list)

join :: Monad m => m (m a) -> m a
join action = action >>= id

ap :: Monad m => m (a -> b) -> m a -> m b
ap function argument = function >>= \f -> argument >>= \a -> pure (f a)

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f action = action >>= \a -> pure (f a)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f first second = first >>= \a -> second >>= \b -> pure (f a b)

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g = \a -> f a >>= g

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) g f = \a -> f a >>= g

-- fmap that evaluates the result before returning it.
(<$!>) :: Monad m => (a -> b) -> m a -> m b
(<$!>) f action = action >>= \a -> let b = f a in seq b (pure b)

class (Alternative m, Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
  mzero = empty
  mplus = (<|>)

instance MonadPlus Maybe

instance MonadPlus []

guard :: Alternative f => Bool -> f ()
guard condition = if condition then pure () else empty

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter keep action = action >>= \a -> if keep a then pure a else mzero

msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero
