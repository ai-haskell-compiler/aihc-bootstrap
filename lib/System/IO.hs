module System.IO where

import Control.Exception (IOException (..), bracket, throwIO)
import Data.IORef

-- A handle is a number that the interpreter knows a file by
-- (src/files.c). Text is read and written as UTF-8. Nothing is buffered
-- beyond what C does, so the buffering functions do nothing.
data Handle = Handle Int

instance Eq Handle where
  (==) (Handle a) (Handle b) = a == b

instance Show Handle where
  show (Handle n) = "<handle " ++ show n ++ ">"

data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
  deriving (Eq, Show)

data BufferMode = NoBuffering | LineBuffering | BlockBuffering (Maybe Int)
  deriving (Eq, Show)

stdin :: Handle
stdin = Handle 0

stdout :: Handle
stdout = Handle 1

stderr :: Handle
stderr = Handle 2

-- ---- Opening and closing ----

openFile :: FilePath -> IOMode -> IO Handle
openFile path mode = handleOpen path (modeNumber mode) >>= \handle ->
  if handle < 0
    then throwIO (IOError (path ++ ": cannot open the file"))
    else pure (Handle handle)
  where
    modeNumber m = case m of
      ReadMode -> 0
      WriteMode -> 1
      AppendMode -> 2
      ReadWriteMode -> 3

openBinaryFile :: FilePath -> IOMode -> IO Handle
openBinaryFile = openFile

hClose :: Handle -> IO ()
hClose (Handle handle) = handleClose handle

-- The file is closed when the action is done, also if it fails.
withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile path mode action = bracket (openFile path mode) hClose action

withBinaryFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withBinaryFile = withFile

-- A new file in the directory, with a name made from the template: the
-- name of the template with a number put before its extension.
openTempFile :: FilePath -> String -> IO (FilePath, Handle)
openTempFile directory template = processId >>= \process -> attempt process (0 :: Int)
  where
    (extensionReversed, nameReversed) = break (== '.') (reverse template)
    (name, extension) =
      if null nameReversed
        then (template, "")
        else (reverse (drop 1 nameReversed), '.' : reverse extensionReversed)
    attempt process n =
      let path = directory ++ "/" ++ name ++ show process ++ "-" ++ show n ++ extension
       in handleOpen path 0 >>= \existing ->
            if existing >= 0
              then handleClose existing >> attempt process (n + 1)
              else fmap (\handle -> (path, handle)) (openFile path ReadWriteMode)

openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile = openTempFile

-- ---- Writing ----

hPutStr :: Handle -> String -> IO ()
hPutStr (Handle descriptor) text = primitiveHPutStr descriptor text

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle text = hPutStr handle text >> hPutStr handle "\n"

hPutChar :: Handle -> Char -> IO ()
hPutChar handle c = hPutStr handle [c]

hPrint :: Show a => Handle -> a -> IO ()
hPrint handle a = hPutStrLn handle (show a)

hFlush :: Handle -> IO ()
hFlush (Handle handle) = handleFlush handle

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering _ _ = pure ()

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering _ = pure LineBuffering

hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode _ _ = pure ()

-- ---- Reading ----

hIsEOF :: Handle -> IO Bool
hIsEOF (Handle handle) = handleIsEOF handle

isEOF :: IO Bool
isEOF = hIsEOF stdin

-- The next line, without its newline.
hGetLine :: Handle -> IO String
hGetLine (Handle handle) = handleRead handle True >>= \found -> case found of
  Just bytes -> pure (bytesToString bytes)
  Nothing -> throwIO (IOError "end of file")

-- Everything that is left. It is read now, not as it is needed.
hGetContents :: Handle -> IO String
hGetContents (Handle handle) = handleRead handle False >>= \found -> case found of
  Just bytes -> pure (bytesToString bytes)
  Nothing -> pure ""

hGetContents' :: Handle -> IO String
hGetContents' = hGetContents

getLine :: IO String
getLine = hGetLine stdin

getContents :: IO String
getContents = hGetContents stdin

interact :: (String -> String) -> IO ()
interact f = getContents >>= \input -> hPutStr stdout (f input)

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

-- The builtins of src/files.c. The bytes are a ByteString, which this
-- module cannot name: Data.ByteString imports it.
data Bytes

foreign import prim "handleOpen" handleOpen :: String -> Int -> IO Int

foreign import prim "handleClose" handleClose :: Int -> IO ()

foreign import prim "handleFlush" handleFlush :: Int -> IO ()

foreign import prim "handleIsEOF" handleIsEOF :: Int -> IO Bool

foreign import prim "handleRead" handleRead :: Int -> Bool -> IO (Maybe Bytes)

foreign import prim "bytesToString" bytesToString :: Bytes -> String

foreign import prim "processId" processId :: IO Int
