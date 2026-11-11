module System.Process where

-- Running a program and waiting for it. Its input comes from a file and
-- its output goes to files, which are read when it has ended: that needs
-- no pipes, and no care about which of them to read first.
--
-- A program that runs alongside the interpreter (createProcess) is not
-- written yet.

import Control.Exception (IOException (..), throwIO)
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (Handle)

data CmdSpec
  = ShellCommand String
  | RawCommand FilePath [String]

data StdStream
  = Inherit
  | UseHandle Handle
  | CreatePipe
  | NoStream

data CreateProcess = CreateProcess
  { cmdspec :: CmdSpec,
    cwd :: Maybe FilePath,
    env :: Maybe [(String, String)],
    std_in :: StdStream,
    std_out :: StdStream,
    std_err :: StdStream
  }

data ProcessHandle = ProcessHandle

proc :: FilePath -> [String] -> CreateProcess
proc program arguments = CreateProcess (RawCommand program arguments) Nothing Nothing Inherit Inherit Inherit

shell :: String -> CreateProcess
shell command = CreateProcess (ShellCommand command) Nothing Nothing Inherit Inherit Inherit

-- The exit code, what the program wrote to standard output, and what it
-- wrote to standard error. The String is its standard input.
readCreateProcessWithExitCode :: CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithExitCode process input = do
  let (program, arguments) = case cmdspec process of
        ShellCommand command -> ("/bin/sh", ["-c", command])
        RawCommand name list -> (name, list)
  case env process of
    Nothing -> pure ()
    Just _ -> interpreterTodo "System.Process: running a program with another environment"
  found <- findExecutable program
  case found of
    Nothing -> throwIO (IOError (program ++ ": the program does not exist"))
    Just _ -> pure ()
  directory <- getTemporaryDirectory
  inputFile <- temporaryFile directory
  outputFile <- temporaryFile directory
  errorFile <- temporaryFile directory
  writeFile inputFile input
  status <- runProcess program arguments (maybe "" id (cwd process)) inputFile outputFile errorFile
  output <- readFile outputFile
  errors <- readFile errorFile
  mapM_ removeFile [inputFile, outputFile, errorFile]
  pure (if status == 0 then ExitSuccess else ExitFailure status, output, errors)

readProcessWithExitCode :: FilePath -> [String] -> String -> IO (ExitCode, String, String)
readProcessWithExitCode program arguments = readCreateProcessWithExitCode (proc program arguments)

-- What the program wrote to standard output. Its standard error is shown,
-- and it is an error if the program fails.
readCreateProcess :: CreateProcess -> String -> IO String
readCreateProcess process input = do
  (code, output, errors) <- readCreateProcessWithExitCode process input
  hPutStrErr errors
  case code of
    ExitSuccess -> pure output
    ExitFailure status -> throwIO (IOError ("the program failed (exit " ++ show status ++ ")"))

readProcess :: FilePath -> [String] -> String -> IO String
readProcess program arguments = readCreateProcess (proc program arguments)

callProcess :: FilePath -> [String] -> IO ()
callProcess program arguments = readProcess program arguments "" >>= putStr

createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess _ = interpreterTodo "System.Process.createProcess"

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess _ = interpreterTodo "System.Process.waitForProcess"

terminateProcess :: ProcessHandle -> IO ()
terminateProcess _ = interpreterTodo "System.Process.terminateProcess"

hPutStrErr :: String -> IO ()
hPutStrErr = primitiveHPutStr 2

foreign import prim "hPutStr" primitiveHPutStr :: Int -> String -> IO ()

foreign import prim "temporaryFile" temporaryFile :: String -> IO String

foreign import prim "runProcess" runProcess :: String -> [String] -> String -> String -> String -> String -> IO Int

foreign import prim "interpreterTodo" interpreterTodo :: String -> a
