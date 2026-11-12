module GHC.IO.Handle where

-- A second handle for the same file. AIHC uses it to give the output of
-- its test runs to several programs, which the interpreter never does.

import System.IO (Handle)

hDuplicate :: Handle -> IO Handle
hDuplicate _ = interpreterTodo "GHC.IO.Handle.hDuplicate"

hDuplicateTo :: Handle -> Handle -> IO ()
hDuplicateTo _ _ = interpreterTodo "GHC.IO.Handle.hDuplicateTo"

foreign import prim "interpreterTodo" interpreterTodo :: String -> a
