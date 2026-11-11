module Main where

import Options.Applicative

data Level = Low | High

data Settings = Settings String Bool [String] (Maybe String) Level Bool

parseLevel :: String -> Either String Level
parseLevel "low" = Right Low
parseLevel "high" = Right High
parseLevel text = Left ("unknown level " ++ text)

settings :: Parser Settings
settings =
  Settings
    <$> strArgument (metavar "INPUT" <> help "The input")
    <*> switch (long "verbose" <> short 'v' <> help "Say more")
    <*> many (strOption (long "include" <> short 'i' <> metavar "DIR" <> help "Add a directory"))
    <*> optional (strOption (long "output" <> short 'o' <> metavar "PATH"))
    <*> option (eitherReader parseLevel) (short 'O' <> metavar "LEVEL" <> value Low)
    <*> (flag False True (long "fast") <|> switch (long "quick"))

data Command = Build Settings | Clean Bool

commands :: Parser Command
commands =
  subparser
    ( command "build" (info (Build <$> settings <**> helper) (progDesc "Build it"))
        <> command "clean" (info (Clean <$> switch (long "all") <**> helper) (progDesc "Clean up"))
    )

showBool :: Bool -> String
showBool True = "yes"
showBool False = "no"

joinItems :: [String] -> String
joinItems [] = ""
joinItems (item : rest) = item ++ ";" ++ joinItems rest

showMaybe :: Maybe String -> String
showMaybe Nothing = "none"
showMaybe (Just text) = text

showLevel :: Level -> String
showLevel Low = "low"
showLevel High = "high"

describe :: Command -> String
describe (Clean everything) = "clean all=" ++ showBool everything
describe (Build (Settings input verbose includes output level fast)) =
  "build " ++ input ++ " verbose=" ++ showBool verbose ++ " includes=" ++ joinItems includes
    ++ " output=" ++ showMaybe output ++ " level=" ++ showLevel level ++ " fast=" ++ showBool fast

main :: IO ()
main = do
  result <- execParser (info (commands <**> helper) (fullDesc <> header "a test"))
  putStrLn (describe result)
