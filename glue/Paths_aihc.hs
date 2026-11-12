module Paths_aihc where

-- Cabal writes this module when it builds AIHC, with the directories of the
-- installation in it. Here the data files are where the sources are:
-- scripts/run-aihc.sh says where in the variable aihc_datadir.

import Data.Version (Version (..))
import System.Environment (lookupEnv)

version :: Version
version = Version [0, 1, 0, 0] []

getDataDir :: IO FilePath
getDataDir = fmap (maybe "." id) (lookupEnv "aihc_datadir")

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = fmap (\directory -> directory ++ "/" ++ name) getDataDir
