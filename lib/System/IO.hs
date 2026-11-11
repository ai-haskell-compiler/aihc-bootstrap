module System.IO where

import Control.Exception (IOException (..), throwIO)

-- A handle is a file descriptor for now: only stdout and stderr.
data Handle = Handle Int

stdout :: Handle
stdout = Handle 1

stderr :: Handle
stderr = Handle 2

hPutStr :: Handle -> String -> IO ()
hPutStr (Handle descriptor) text = primitiveHPutStr descriptor text

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle text = hPutStr handle text >> hPutStr handle "\n"

hPrint :: Show a => Handle -> a -> IO ()
hPrint handle a = hPutStrLn handle (show a)

-- Output is not buffered by the interpreter beyond what C does.
hFlush :: Handle -> IO ()
hFlush _ = pure ()

-- Files are read and written whole, as UTF-8.
readFile :: FilePath -> IO String
readFile path = stringReadFile path >>= \found -> case found of
  Just text -> pure text
  Nothing -> throwIO (IOError (path ++ ": cannot read the file"))

writeFile :: FilePath -> String -> IO ()
writeFile path text = stringWriteFile False path text >>= \ok ->
  if ok then pure () else throwIO (IOError (path ++ ": cannot write the file"))

appendFile :: FilePath -> String -> IO ()
appendFile path text = stringWriteFile True path text >>= \ok ->
  if ok then pure () else throwIO (IOError (path ++ ": cannot write the file"))

foreign import prim "hPutStr" primitiveHPutStr :: Int -> String -> IO ()

foreign import prim "stringReadFile" stringReadFile :: String -> IO (Maybe String)

foreign import prim "stringWriteFile" stringWriteFile :: Bool -> String -> String -> IO Bool
