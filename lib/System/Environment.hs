module System.Environment where

foreign import prim "getArgs" getArgs :: IO [String]

foreign import prim "lookupEnv" lookupEnv :: String -> IO (Maybe String)

getEnv :: String -> IO String
getEnv name = lookupEnv name >>= \found -> case found of
  Just value -> pure value
  Nothing -> ioError (userError (name ++ ": the environment variable is not set"))
