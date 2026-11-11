module System.IO.Error where

import Control.Exception (IOException (..), catch, throwIO, try)

type IOError = IOException

ioError :: IOError -> IO a
ioError = throwIO

userError :: String -> IOError
userError message = IOError ("user error (" ++ message ++ ")")

tryIOError :: IO a -> IO (Either IOError a)
tryIOError = try

catchIOError :: IO a -> (IOError -> IO a) -> IO a
catchIOError = catch

modifyIOError :: (IOError -> IOError) -> IO a -> IO a
modifyIOError f action = catch action (\e -> throwIO (f e))

ioeGetErrorString :: IOError -> String
ioeGetErrorString (IOError message) = message
