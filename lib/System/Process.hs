module System.Process where

-- Running a program and waiting for it. Its input comes from a file and
-- its output goes to files, which are read when it has ended: that needs
-- no pipes, and no care about which of them to read first.
--
-- createProcess too runs the program to its end before it returns; what
-- it wrote is then copied to where it should have gone. That serves a
-- caller that waits for the program at once, as hsc2hs does, but not one
-- that talks to the program while it runs.

import Control.Exception (IOException (..), throwIO)
import qualified Data.ByteString as B
import Data.Char (isAlphaNum)
import System.Directory (findExecutable, getTemporaryDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.IO (Handle, IOMode (..), openFile, stderr, stdout)

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
    std_err :: StdStream,
    -- Windows only: whether to wait for the program's own children too.
    use_process_jobs :: Bool
  }

-- The program has ended when there is a handle: this is how it ended.
data ProcessHandle = ProcessHandle ExitCode

proc :: FilePath -> [String] -> CreateProcess
proc program arguments = CreateProcess (RawCommand program arguments) Nothing Nothing Inherit Inherit Inherit False

shell :: String -> CreateProcess
shell command = CreateProcess (ShellCommand command) Nothing Nothing Inherit Inherit Inherit False

-- The exit code, what the program wrote to standard output, and what it
-- wrote to standard error. The String is its standard input.
readCreateProcessWithExitCode :: CreateProcess -> String -> IO (ExitCode, String, String)
readCreateProcessWithExitCode process input = do
  let (program, arguments) = case cmdspec process of
        ShellCommand command -> ("/bin/sh", ["-c", command])
        RawCommand name list -> (name, list)
  found <- findExecutable program
  case found of
    Nothing -> throwIO (IOError (program ++ ": the program does not exist"))
    Just _ -> pure ()
  directory <- getTemporaryDirectory
  inputFile <- temporaryFile directory
  outputFile <- temporaryFile directory
  errorFile <- temporaryFile directory
  writeFile inputFile input
  let (replace, variables) = case env process of
        Nothing -> (False, [])
        Just list -> (True, map (\(name, value) -> name ++ "=" ++ value) list)
  status <- primitiveRunProcess program arguments (maybe "" id (cwd process)) inputFile outputFile errorFile replace variables
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

-- Standard input can only be inherited, and the program gets none.
createProcess :: CreateProcess -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
createProcess process = do
  case std_in process of
    Inherit -> pure ()
    _ -> interpreterTodo "System.Process.createProcess with a standard input"
  let (program, arguments) = case cmdspec process of
        ShellCommand command -> ("/bin/sh", ["-c", command])
        RawCommand name list -> (name, list)
      (replace, variables) = case env process of
        Nothing -> (False, [])
        Just list -> (True, map (\(name, value) -> name ++ "=" ++ value) list)
  directory <- getTemporaryDirectory
  inputFile <- temporaryFile directory
  outputFile <- temporaryFile directory
  errorFile <- temporaryFile directory
  writeFile inputFile ""
  status <- primitiveRunProcess program arguments (maybe "" id (cwd process)) inputFile outputFile errorFile replace variables
  output <- deliver (std_out process) stdout outputFile
  errors <- deliver (std_err process) stderr errorFile
  removeFile inputFile
  pure (Nothing, output, errors, ProcessHandle (if status == 0 then ExitSuccess else ExitFailure status))
  where
    -- What the program wrote to the file goes where the stream says. A
    -- pipe is the file itself, open for reading; it is removed at once,
    -- which a file that is open survives.
    deliver stream inherited file = case stream of
      CreatePipe -> do
        handle <- openFile file ReadMode
        removeFile file
        pure (Just handle)
      UseHandle handle -> copy handle file >> pure Nothing
      Inherit -> copy inherited file >> pure Nothing
      NoStream -> removeFile file >> pure Nothing
    copy handle file = B.readFile file >>= B.hPut handle >> removeFile file

-- The program with the given standard output and error, as in process.
runProcess :: FilePath -> [String] -> Maybe FilePath -> Maybe [(String, String)] -> Maybe Handle -> Maybe Handle -> Maybe Handle -> IO ProcessHandle
runProcess program arguments directory environment input output errors = do
  let stream = maybe Inherit UseHandle
  (_, _, _, handle) <- createProcess (proc program arguments) {cwd = directory, env = environment, std_in = stream input, std_out = stream output, std_err = stream errors}
  pure handle

waitForProcess :: ProcessHandle -> IO ExitCode
waitForProcess (ProcessHandle code) = pure code

terminateProcess :: ProcessHandle -> IO ()
terminateProcess _ = interpreterTodo "System.Process.terminateProcess"

-- The command line as a POSIX shell would take it, as in process.
showCommandForUser :: FilePath -> [String] -> String
showCommandForUser command arguments = unwords (map translate (command : arguments))
  where
    translate "" = "''"
    translate text
      | all good text = text
      | otherwise = '\'' : foldr escape "'" text
    escape '\'' = showString "'\\''"
    escape c = showChar c
    good c = isAlphaNum c || c `elem` "-_.,/:@"

hPutStrErr :: String -> IO ()
hPutStrErr = primitiveHPutStr 2

foreign import prim "hPutStr" primitiveHPutStr :: Int -> String -> IO ()

foreign import prim "temporaryFile" temporaryFile :: String -> IO String

foreign import prim "runProcess" primitiveRunProcess :: String -> [String] -> String -> String -> String -> String -> Bool -> [String] -> IO Int

foreign import prim "interpreterTodo" interpreterTodo :: String -> a
