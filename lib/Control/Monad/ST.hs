module Control.Monad.ST where

-- ST is IO whose effects cannot be seen from outside. The interpreter does
-- not check that, so runST takes any ST action.

import System.IO.Unsafe (unsafePerformIO)

newtype ST s a = ST (IO a)

data RealWorld

instance Functor (ST s) where
  fmap f (ST action) = ST (fmap f action)

instance Applicative (ST s) where
  pure a = ST (pure a)
  (<*>) (ST function) (ST argument) = ST (function <*> argument)

instance Monad (ST s) where
  (>>=) (ST action) next = ST (action >>= \a -> case next a of
    ST continue -> continue)

runST :: ST s a -> a
runST (ST action) = unsafePerformIO action

stToIO :: ST RealWorld a -> IO a
stToIO (ST action) = action

unsafeIOToST :: IO a -> ST s a
unsafeIOToST = ST
