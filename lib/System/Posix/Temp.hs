module System.Posix.Temp where

import System.Directory (createDirectory, doesPathExist)

-- A new directory whose name is the prefix and a number.
mkdtemp :: FilePath -> IO FilePath
mkdtemp prefix = processId >>= \process -> attempt process (0 :: Int)
  where
    attempt process n =
      let path = prefix ++ show process ++ "-" ++ show n
       in doesPathExist path >>= \exists ->
            if exists then attempt process (n + 1) else createDirectory path >> pure path

foreign import prim "processId" processId :: IO Int
