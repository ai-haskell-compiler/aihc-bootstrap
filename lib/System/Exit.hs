module System.Exit where

import Control.Exception (Exception, throwIO)

data ExitCode = ExitSuccess | ExitFailure Int

instance Show ExitCode where
  show code = case code of
    ExitSuccess -> "ExitSuccess"
    ExitFailure _ -> "ExitFailure"

instance Exception ExitCode

exitWith :: ExitCode -> IO a
exitWith code = throwIO code

exitFailure :: IO a
exitFailure = exitWith (ExitFailure 1)

exitSuccess :: IO a
exitSuccess = exitWith ExitSuccess
