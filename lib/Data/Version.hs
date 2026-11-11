module Data.Version where

import Data.List (intercalate)

-- A version: its numbers, and tags that nobody uses any more.
data Version = Version {versionBranch :: [Int], versionTags :: [String]}
  deriving (Eq, Ord, Show)

-- 1.2.3
showVersion :: Version -> String
showVersion (Version branch tags) = intercalate "." (map show branch) ++ concatMap ('-' :) tags

makeVersion :: [Int] -> Version
makeVersion branch = Version branch []
