module Control.Exception where

-- Any exception, together with its type.
data SomeException = forall e. Exception e => SomeException e

class Show e => Exception e where
  toException :: e -> SomeException
  fromException :: SomeException -> Maybe e
  displayException :: e -> String
  toException = SomeException
  fromException (SomeException e) = cast e
  displayException = show

instance Exception SomeException where
  toException exception = exception
  fromException exception = Just exception
  displayException (SomeException e) = displayException e

data IOException = IOError String

instance Show IOException where
  show (IOError message) = message

instance Exception IOException

-- What `error` throws.
data ErrorCall = ErrorCall String

instance Show ErrorCall where
  show (ErrorCall message) = message

instance Exception ErrorCall

throw :: Exception e => e -> a
throw e = raise (toException e)

throwIO :: Exception e => e -> IO a
throwIO e = IO (\world -> raise (toException e))

-- Runs the handler if the action raises an exception of the handler's type.
catch :: Exception e => IO a -> (e -> IO a) -> IO a
catch action handler = catchAny action (\exception -> case fromException exception of
  Just e -> handler e
  Nothing -> throwIO exception)

try :: Exception e => IO a -> IO (Either e a)
try action = catch (fmap Right action) (\e -> pure (Left e))

foreign import prim "raise" raise :: SomeException -> a

foreign import prim "catchAny" catchAny :: IO a -> (SomeException -> IO a) -> IO a

-- Just the argument if both types are the same.
foreign import prim "cast" cast :: a -> Maybe b

-- Evaluates the value now, so that an exception in it is raised here.
evaluate :: a -> IO a
evaluate a = seq a (pure a)

handle :: Exception e => (e -> IO a) -> IO a -> IO a
handle handler action = catch action handler

-- Runs the second action if the first raises an exception, and raises the
-- exception again.
onException :: IO a -> IO b -> IO a
onException action cleanup = catchAny action (\exception -> cleanup >> throwIO exception)

-- Runs the second action after the first, whether or not it raises.
finally :: IO a -> IO b -> IO a
finally action cleanup = onException action cleanup >>= \a -> cleanup >> pure a

-- Acquires something, uses it, and releases it whatever happens.
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release use = acquire >>= \resource -> finally (use resource) (release resource)

bracket_ :: IO a -> IO b -> IO c -> IO c
bracket_ acquire release use = bracket acquire (\_ -> release) (\_ -> use)

bracketOnError :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracketOnError acquire release use = acquire >>= \resource -> onException (use resource) (release resource)

-- Nothing interrupts the interpreter from outside, so masking does nothing.
mask :: ((IO b -> IO b) -> IO a) -> IO a
mask action = action id

mask_ :: IO a -> IO a
mask_ action = action

uninterruptibleMask_ :: IO a -> IO a
uninterruptibleMask_ action = action

-- Never raised: there are no asynchronous exceptions.
data SomeAsyncException = SomeAsyncException

instance Show SomeAsyncException where
  show _ = "SomeAsyncException"

instance Exception SomeAsyncException

-- An exception a program can raise without declaring a type for it.
data ArithException = DivideByZero | Overflow

instance Show ArithException where
  show DivideByZero = "divide by zero"
  show Overflow = "arithmetic overflow"

instance Exception ArithException

-- What a thread gets when it waits for something that no thread can give
-- it. With one thread, that is every wait.
data BlockedIndefinitelyOnMVar = BlockedIndefinitelyOnMVar

instance Show BlockedIndefinitelyOnMVar where
  show _ = "thread blocked indefinitely in an MVar operation"

instance Exception BlockedIndefinitelyOnMVar

data BlockedIndefinitelyOnSTM = BlockedIndefinitelyOnSTM

instance Show BlockedIndefinitelyOnSTM where
  show _ = "thread blocked indefinitely in an STM transaction"

instance Exception BlockedIndefinitelyOnSTM
