module System.Environment where

import System.Directory (canonicalizePath, findExecutable)

foreign import prim "getArgs" getArgs :: IO [String]

foreign import prim "lookupEnv" lookupEnv :: String -> IO (Maybe String)

getEnv :: String -> IO String
getEnv name = lookupEnv name >>= \found -> case found of
  Just value -> pure value
  Nothing -> ioError (userError (name ++ ": the environment variable is not set"))

-- Every variable, with its value.
getEnvironment :: IO [(String, String)]
getEnvironment = fmap (map split) environment
  where
    split entry = case break (== '=') entry of
      (name, value) -> (name, drop 1 value)

-- The path of the program that is running. Under the interpreter that is
-- the interpreter, not the program it runs.
getExecutablePath :: IO FilePath
getExecutablePath = programPath >>= \path ->
  if elem '/' path
    then canonicalizePath path
    else findExecutable path >>= \found -> maybe (pure path) canonicalizePath found

getProgName :: IO String
getProgName = fmap (reverse . takeWhile (/= '/') . reverse) programPath

foreign import prim "environment" environment :: IO [String]

foreign import prim "programPath" programPath :: IO String
