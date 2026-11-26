module Paths_hsc2hs where

-- Cabal writes this module when it builds hsc2hs. Here the data file,
-- template-hsc.h, is where the sources are: scripts/hsc2hs.sh says where
-- in the variable hsc2hs_datadir.

import Data.Version (Version (..))
import System.Environment (lookupEnv)

version :: Version
version = Version [0, 68, 10] []

getDataDir :: IO FilePath
getDataDir = fmap (maybe "." id) (lookupEnv "hsc2hs_datadir")

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = fmap (\directory -> directory ++ "/" ++ name) getDataDir
