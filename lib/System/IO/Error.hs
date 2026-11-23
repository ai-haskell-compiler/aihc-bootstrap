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

isUserError :: IOError -> Bool
isUserError (IOError message) = take 12 message == "user error ("

-- For an error of userError, the message that was given to it, as in base.
ioeGetErrorString :: IOError -> String
ioeGetErrorString (IOError message) =
  if take 12 message == "user error (" && not (null message)
    then init (drop 12 message)
    else message

-- The interpreter's files are never locked against the process itself.
isAlreadyInUseError :: IOError -> Bool
isAlreadyInUseError _ = False
