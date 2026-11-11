module Main where

import Control.Exception (IOException, try)
import Control.Monad (forM_)
import Data.List (isSuffixOf, sort)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.Process

-- Whether the action fails with an IOException. The messages differ from
-- GHC's, so they are not printed.
failsWith :: IO a -> IO Bool
failsWith action = do
  result <- try (action >> pure ()) :: IO (Either IOException ())
  pure (either (const True) (const False) result)

main :: IO ()
main = do
  temporary <- getTemporaryDirectory
  let root = temporary </> "aihc-bootstrap-test-system"
  removePathForcibly root
  print =<< doesPathExist root

  createDirectory root
  print =<< doesDirectoryExist root
  print =<< doesFileExist root
  print =<< failsWith (createDirectory root)
  createDirectoryIfMissing True (root </> "a" </> "b" </> "c")
  createDirectoryIfMissing True (root </> "a" </> "b" </> "c")
  createDirectoryIfMissing False (root </> "d/")
  print =<< failsWith (createDirectoryIfMissing False (root </> "x" </> "y"))
  print . sort =<< listDirectory root
  print . sort =<< listDirectory (root </> "a")
  print =<< listDirectory (root </> "a" </> "b" </> "c")
  print . sort =<< getDirectoryContents (root </> "a")
  print =<< failsWith (listDirectory (root </> "missing"))

  writeFile (root </> "one.txt") "first\n"
  writeFile (root </> "a" </> "two.txt") "second file\n"
  print =<< doesFileExist (root </> "one.txt")
  print =<< doesDirectoryExist (root </> "one.txt")
  print =<< getFileSize (root </> "a" </> "two.txt")
  print =<< failsWith (getFileSize (root </> "missing"))
  copyFile (root </> "one.txt") (root </> "copy.txt")
  putStr =<< readFile (root </> "copy.txt")
  renameFile (root </> "copy.txt") (root </> "renamed.txt")
  print =<< mapM (doesFileExist . (root </>)) ["copy.txt", "renamed.txt"]
  renameDirectory (root </> "d") (root </> "e")
  print . sort =<< listDirectory root
  removeFile (root </> "renamed.txt")
  print =<< failsWith (removeFile (root </> "renamed.txt"))
  print =<< failsWith (removeDirectory (root </> "a"))
  removeDirectory (root </> "e")
  print . sort =<< listDirectory root

  -- Paths.
  canonical <- canonicalizePath root
  print (isAbsolute canonical, takeFileName canonical)
  viaDots <- canonicalizePath (root </> "a" </> ".." </> "a" </> "." </> "b")
  print (makeRelative canonical viaDots)
  here <- getCurrentDirectory
  print (isAbsolute here)
  absolute <- makeAbsolute ("some" </> "." </> "file")
  print (makeRelative here absolute)
  print =<< makeAbsolute "/already//absolute/./x"
  home <- getHomeDirectory
  print (isAbsolute home)
  cache <- getXdgDirectory XdgCache ("aihc" </> "store")
  print (isAbsolute cache, "aihc/store" `isSuffixOf` cache)

  -- Programs.
  print . fmap takeFileName =<< findExecutable "sh"
  print =<< findExecutable "no-such-program-anywhere"
  print =<< readProcessWithExitCode "sh" ["-c", "echo out; echo err >&2; exit 3"] ""
  print =<< readProcessWithExitCode "cat" [] "line one\nline two\n"
  print =<< readProcessWithExitCode "sh" ["-c", "printf '%s|' \"$@\"", "name", "a b", "", "c'd", "é"] ""
  print =<< readProcessWithExitCode "tr" ["a-z", "A-Z"] "héllo"
  print =<< readProcessWithExitCode "sh" ["-c", "kill -9 $$"] ""
  print =<< readCreateProcessWithExitCode ((proc "ls" []) {cwd = Just (root </> "a")}) ""
  print =<< readCreateProcessWithExitCode (shell "echo $((6 * 7)) && false") ""
  putStr =<< readProcess "sh" ["-c", "cat; echo done"] "given\n"
  print =<< failsWith (readProcess "sh" ["-c", "exit 1"] "")
  print =<< failsWith (readProcessWithExitCode "no-such-program-anywhere" [] "")
  forM_ [ExitSuccess, ExitFailure 2] print
  print (ExitFailure 2 == ExitFailure 2, ExitSuccess == ExitFailure 0)

  removeDirectoryRecursive root
  print =<< doesPathExist root
  removePathForcibly root
