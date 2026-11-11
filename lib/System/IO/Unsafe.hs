module System.IO.Unsafe where

-- Runs the action when the result is first needed.
unsafePerformIO :: IO a -> a
unsafePerformIO (IO action) = case action realWorld of
  IOResult a -> a

unsafeDupablePerformIO :: IO a -> a
unsafeDupablePerformIO = unsafePerformIO

foreign import prim "realWorld" realWorld :: World
