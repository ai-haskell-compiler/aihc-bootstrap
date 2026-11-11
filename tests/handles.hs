module Main where

import Control.Exception (IOException, try)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.FilePath (takeDirectory, takeExtension, (</>))
import System.IO

main :: IO ()
main = do
  temporary <- getTemporaryDirectory
  let path = temporary </> "aihc-bootstrap-test-handles.txt"
  -- Writing through a handle, in pieces.
  withFile path WriteMode (\handle -> do
    hPutStr handle "first"
    hPutStrLn handle " line"
    hPutChar handle 'x'
    hPrint handle (1 :: Int, "two")
    TIO.hPutStrLn handle (T.pack "text λ")
    B.hPut handle (BC.pack "bytes\n")
    hFlush handle)
  putStr =<< readFile path
  -- Adding to the end.
  withFile path AppendMode (\handle -> hPutStr handle "appended\nno newline at the end")
  -- Reading it back line by line.
  handle <- openFile path ReadMode
  print =<< hIsEOF handle
  first <- hGetLine handle
  second <- hGetLine handle
  print (first, second)
  third <- TIO.hGetLine handle
  print (T.unpack third)
  fourth <- B.hGetLine handle
  print (B.unpack fourth)
  fifth <- hGetLine handle
  print fifth
  print =<< hIsEOF handle
  rest <- hGetContents' handle
  print rest
  atEnd <- try (hGetLine handle) :: IO (Either IOException String)
  putStrLn (either (const "end of file") id atEnd)
  hClose handle
  hClose handle
  -- Reading whole, as text and as bytes.
  print . T.length =<< withFile path ReadMode TIO.hGetContents
  print . B.length =<< withFile path ReadMode B.hGetContents
  print . lines =<< withFile path ReadMode hGetContents'
  -- A file that is not there, and a handle that is closed after a failure.
  missing <- try (openFile (temporary </> "aihc-bootstrap-no-such-file") ReadMode) :: IO (Either IOException Handle)
  putStrLn (either (const "cannot open") (const "opened") missing)
  failed <- try (withFile path WriteMode (\h -> hPutStr h "partial" >> ioError (userError "stop"))) :: IO (Either IOException ())
  putStrLn (either (const "failed") (const "done") failed)
  print =<< readFile path
  -- Temporary files: a new name each time, in the directory, with the
  -- extension of the template.
  (one, handleOne) <- openTempFile temporary "aihc-bootstrap-test.tmp"
  (two, handleTwo) <- openBinaryTempFile temporary "aihc-bootstrap-test.o"
  hPutStr handleOne "one"
  B.hPut handleTwo (B.pack [0, 255, 10])
  hClose handleOne
  hClose handleTwo
  print (one /= two, takeDirectory one == takeDirectory (temporary </> "x"), takeExtension one, takeExtension two)
  print =<< readFile one
  print . B.unpack =<< B.readFile two
  mapM_ removeFile [one, two, path]
  print =<< mapM doesFileExist [one, two, path]
  -- The standard handles.
  hSetBuffering stdout NoBuffering
  hPutStrLn stdout "to stdout"
  print (stdout == stdout, stdout == stderr)
