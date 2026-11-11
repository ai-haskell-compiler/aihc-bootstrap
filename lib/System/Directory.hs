module System.Directory where

-- Files and directories, over the builtins of src/posix.c. A failure is an
-- IOException, as upstream, but with a message of its own.

import Control.Exception (IOException (..), throwIO)
import Control.Monad (filterM, unless, when)
import qualified Data.ByteString as B
import System.Environment (lookupEnv)
import System.FilePath

data XdgDirectory = XdgData | XdgConfig | XdgCache | XdgState

failed :: String -> FilePath -> IO a
failed what path = throwIO (IOError (path ++ ": " ++ what))

-- Does the action, which says whether it worked.
checked :: String -> FilePath -> IO Bool -> IO ()
checked what path action = action >>= \ok -> unless ok (failed what path)

-- ---- Questions ----

doesFileExist :: FilePath -> IO Bool
doesFileExist path = fmap (== 2) (pathKind path)

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist path = fmap (== 1) (pathKind path)

doesPathExist :: FilePath -> IO Bool
doesPathExist path = fmap (/= 0) (pathKind path)

getFileSize :: FilePath -> IO Integer
getFileSize path = fileSize path >>= \size ->
  if size < 0 then failed "cannot find the size of the file" path else pure (toInteger size)

-- The names in the directory, without "." and "..", in no particular order.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path = primitiveListDirectory path >>= \found -> case found of
  Just names -> pure names
  Nothing -> failed "cannot list the directory" path

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents path = fmap (\names -> "." : ".." : names) (listDirectory path)

-- ---- Changes ----

createDirectory :: FilePath -> IO ()
createDirectory path = checked "cannot create the directory" path (makeDirectory path)

-- With True, the directories above it are created too.
createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing parents path = do
  exists <- doesDirectoryExist path
  unless exists $ do
    let parent = takeDirectory (dropTrailingPathSeparator path)
    when (parents && parent /= path) (createDirectoryIfMissing True parent)
    createDirectory path

removeFile :: FilePath -> IO ()
removeFile path = checked "cannot remove the file" path (removePath path)

removeDirectory :: FilePath -> IO ()
removeDirectory path = checked "cannot remove the directory" path (removePath path)

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive path = do
  names <- listDirectory path
  mapM_ (\name -> removePathForcibly (path </> name)) names
  removeDirectory path

-- Removes the file or the directory with everything in it, if it exists.
removePathForcibly :: FilePath -> IO ()
removePathForcibly path = pathKind path >>= \kind -> case kind of
  0 -> pure ()
  1 -> removeDirectoryRecursive path
  _ -> removeFile path

renameFile :: FilePath -> FilePath -> IO ()
renameFile from to = checked "cannot rename the file" from (renamePath from to)

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory from to = checked "cannot rename the directory" from (renamePath from to)

renamePath' :: FilePath -> FilePath -> IO ()
renamePath' from to = checked "cannot rename" from (renamePath from to)

copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = B.readFile from >>= B.writeFile to

-- ---- Places ----

getCurrentDirectory :: IO FilePath
getCurrentDirectory = currentDirectory

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute path =
  if isAbsolute path
    then pure (normalise path)
    else fmap (\directory -> normalise (directory </> path)) getCurrentDirectory

-- The path with symbolic links, "." and ".." resolved, if it exists.
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath path = realPath path >>= \found -> case found of
  Just resolved -> pure resolved
  Nothing -> makeAbsolute path

getHomeDirectory :: IO FilePath
getHomeDirectory = lookupEnv "HOME" >>= \found -> case found of
  Just home -> pure home
  Nothing -> failed "the environment variable is not set" "HOME"

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = fmap (maybe "/tmp" id) (lookupEnv "TMPDIR")

-- The directory for the kind of file, with the path added to it. The
-- environment variable wins if it holds an absolute path.
getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory kind suffix = do
  home <- getHomeDirectory
  let (variable, fallback) = case kind of
        XdgData -> ("XDG_DATA_HOME", ".local/share")
        XdgConfig -> ("XDG_CONFIG_HOME", ".config")
        XdgCache -> ("XDG_CACHE_HOME", ".cache")
        XdgState -> ("XDG_STATE_HOME", ".local/state")
  found <- lookupEnv variable
  let base = case found of
        Just path | isAbsolute path -> path
        _ -> home </> fallback
  pure (normalise (base </> suffix))

-- Where the program is: the name itself if it has a slash in it, otherwise
-- the first directory of PATH that has it.
findExecutable :: String -> IO (Maybe FilePath)
findExecutable name = do
  path <- lookupEnv "PATH"
  let candidates =
        if any isPathSeparator name
          then [name]
          else [directory </> name | directory <- splitSearchPath (maybe "" id path)]
  found <- filterM isExecutableFile candidates
  pure (case found of
    [] -> Nothing
    first : _ -> Just first)

isExecutableFile :: FilePath -> IO Bool
isExecutableFile path = do
  file <- doesFileExist path
  if file then isExecutable path else pure False

foreign import prim "pathKind" pathKind :: String -> IO Int

foreign import prim "fileSize" fileSize :: String -> IO Int

foreign import prim "isExecutable" isExecutable :: String -> IO Bool

foreign import prim "listDirectory" primitiveListDirectory :: String -> IO (Maybe [String])

foreign import prim "makeDirectory" makeDirectory :: String -> IO Bool

foreign import prim "removePath" removePath :: String -> IO Bool

foreign import prim "renamePath" renamePath :: String -> String -> IO Bool

foreign import prim "currentDirectory" currentDirectory :: IO String

foreign import prim "realPath" realPath :: String -> IO (Maybe String)
