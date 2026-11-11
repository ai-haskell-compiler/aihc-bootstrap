module System.Environment where

foreign import prim "getArgs" getArgs :: IO [String]

foreign import prim "lookupEnv" lookupEnv :: String -> IO (Maybe String)
