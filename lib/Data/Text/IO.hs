module Data.Text.IO where

-- Files are read and written as UTF-8, which is what a Text is.

import qualified Data.ByteString as B
import Data.Text (Text (..), pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import System.IO (Handle)
import qualified System.IO as IO

readFile :: FilePath -> IO Text
readFile path = fmap decodeUtf8 (B.readFile path)

writeFile :: FilePath -> Text -> IO ()
writeFile path (Text bytes) = B.writeFile path bytes

appendFile :: FilePath -> Text -> IO ()
appendFile path text = IO.appendFile path (unpack text)

putStr :: Text -> IO ()
putStr = hPutStr IO.stdout

putStrLn :: Text -> IO ()
putStrLn = hPutStrLn IO.stdout

hPutStr :: Handle -> Text -> IO ()
hPutStr handle (Text bytes) = B.hPut handle bytes

hPutStrLn :: Handle -> Text -> IO ()
hPutStrLn handle text = hPutStr handle text >> IO.hPutStr handle "\n"

hGetLine :: Handle -> IO Text
hGetLine handle = fmap pack (IO.hGetLine handle)

hGetContents :: Handle -> IO Text
hGetContents handle = fmap pack (IO.hGetContents handle)

getLine :: IO Text
getLine = hGetLine IO.stdin

getContents :: IO Text
getContents = hGetContents IO.stdin
