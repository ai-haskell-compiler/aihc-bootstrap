module TopHandler where

import Control.Exception (SomeException, catchAny, displayException, fromException)
import System.Exit (ExitCode (..))
import System.IO (hPutStrLn, stderr)

-- What the interpreter runs: main, and then what to do with an exception
-- that main did not catch.
runMain :: IO a -> IO ()
runMain action = catchAny (action >> pure ()) uncaught

uncaught :: SomeException -> IO ()
uncaught exception = case fromException exception of
  Just code -> exit (exitStatus code)
  Nothing -> hPutStrLn stderr (displayException exception) >> exit 1

exitStatus :: ExitCode -> Int
exitStatus code = case code of
  ExitSuccess -> 0
  ExitFailure status -> status

foreign import prim "exit" exit :: Int -> IO a
