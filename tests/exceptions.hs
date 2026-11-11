module Main where

import Control.Exception
import System.Exit (ExitCode (..), exitWith)

data Failure = Failure String

instance Show Failure where
  show (Failure message) = "Failure: " ++ message

instance Exception Failure

data Other = Other

instance Show Other where
  show Other = "Other"

instance Exception Other

describe :: Either Failure () -> String
describe result = case result of
  Left failure -> "caught " ++ displayException failure
  Right () -> "no exception"

-- Raises only when the string is looked at.
lazyFailure :: String
lazyFailure = throw (Failure "thrown in pure code")

main :: IO ()
main = do
  first <- try (putStrLn "running")
  putStrLn (describe first)
  second <- try (throwIO (Failure "thrown in IO"))
  putStrLn (describe second)
  third <- try (putStrLn lazyFailure)
  putStrLn (describe third)
  -- The same thunk raises again when it is evaluated again.
  again <- try (putStrLn lazyFailure)
  putStrLn (describe again)
  -- A handler for another type does not catch it; the outer one does.
  fourth <- try (catch (throwIO (Failure "passed on")) (\Other -> putStrLn "wrong handler"))
  putStrLn (describe fourth)
  -- A handler for SomeException catches everything.
  catch (throwIO Other) (\e -> case (e :: SomeException) of
    SomeException inner -> putStrLn ("any: " ++ show inner))
  -- Leaving through an exception that nothing catches.
  putStrLn "exiting"
  exitWith (ExitFailure 3)
  putStrLn "not reached"
