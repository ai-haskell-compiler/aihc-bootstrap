module Data.Text.IO where

-- Files are read and written as UTF-8.

import Data.Text (Text (..))
import System.IO (Handle)
import qualified System.IO as IO

readFile :: FilePath -> IO Text
readFile path = fmap Text (IO.readFile path)

writeFile :: FilePath -> Text -> IO ()
writeFile path (Text string) = IO.writeFile path string

appendFile :: FilePath -> Text -> IO ()
appendFile path (Text string) = IO.appendFile path string

putStr :: Text -> IO ()
putStr (Text string) = IO.hPutStr IO.stdout string

putStrLn :: Text -> IO ()
putStrLn (Text string) = IO.hPutStrLn IO.stdout string

hPutStr :: Handle -> Text -> IO ()
hPutStr handle (Text string) = IO.hPutStr handle string

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn handle (Text string) = IO.hPutStrLn handle string
