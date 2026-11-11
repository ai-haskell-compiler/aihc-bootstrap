module Options.Applicative where

-- A small replacement for the optparse-applicative package. It has only the
-- names AIHC uses, and it prints simpler help and error messages.
--
-- A parser works in two passes over the arguments, because options may come
-- in any order, before or after the positional arguments:
--   pass one takes the options the parser knows out of the list;
--   pass two takes positional arguments from what is left.

import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

-- The outcome of running a parser on a list of arguments.
data Result a
  = Failed String
  | -- --help was given; the nearest runInfo turns this into a HelpText.
    HelpRequested
  | HelpText String
  | -- Whether any argument was taken, the value, and the arguments left.
    Parsed Bool a [String]

-- Pass two of a parser.
newtype Positional a = Positional ([String] -> Result a)

-- What --help prints about the parser, and pass one.
data Parser a = Parser [Item] ([String] -> Result (Positional a))

-- One line of help: what to type, and what it does.
data Item = Item String String

data ParserInfo a = ParserInfo (Parser a) String String

-- Continues with a parsed value; anything else is passed on.
andThen :: Result a -> (Bool -> a -> [String] -> Result b) -> Result b
andThen (Failed message) _ = Failed message
andThen HelpRequested _ = HelpRequested
andThen (HelpText text) _ = HelpText text
andThen (Parsed taken a rest) next = next taken a rest

purePositional :: a -> Positional a
purePositional a = Positional (\arguments -> Parsed False a arguments)

mapPositional :: (a -> b) -> Positional a -> Positional b
mapPositional f (Positional second) = Positional (\arguments ->
  andThen (second arguments) (\taken a rest -> Parsed taken (f a) rest))

applyPositional :: Positional (a -> b) -> Positional a -> Positional b
applyPositional (Positional function) (Positional argument) = Positional (\arguments ->
  andThen (function arguments) (\taken f rest ->
    andThen (argument rest) (\otherTaken a final ->
      Parsed (taken || otherTaken) (f a) final)))

instance Functor Parser where
  fmap f (Parser items first) = Parser items (\arguments ->
    andThen (first arguments) (\taken second rest ->
      Parsed taken (mapPositional f second) rest))

instance Applicative Parser where
  pure a = Parser [] (\arguments -> Parsed False (purePositional a) arguments)
  (<*>) (Parser items function) (Parser otherItems argument) = Parser (items ++ otherItems) (\arguments ->
    andThen (function arguments) (\taken f rest ->
      andThen (argument rest) (\otherTaken a final ->
        Parsed (taken || otherTaken) (applyPositional f a) final)))

infixl 4 <**>

(<**>) :: Parser a -> Parser (a -> b) -> Parser b
(<**>) argument function = (\a f -> f a) <$> argument <*> function

infixl 3 <|>

-- Whichever parser takes something from the arguments, the first one
-- first. If neither does, the first one that succeeds with nothing.
(<|>) :: Parser a -> Parser a -> Parser a
(<|>) (Parser items first) (Parser otherItems other) = Parser (items ++ otherItems) (\arguments ->
  case first arguments of
    Failed _ -> other arguments
    Parsed False second rest -> case other arguments of
      Parsed True otherSecond otherRest -> Parsed True otherSecond otherRest
      _ -> Parsed False second rest
    result -> result)

-- The parser as often as it takes something from the arguments.
many :: Parser a -> Parser [a]
many (Parser items first) = Parser items (manyFirst first)

manyFirst :: ([String] -> Result (Positional a)) -> [String] -> Result (Positional [a])
manyFirst first arguments = case first arguments of
  Failed _ -> Parsed False (purePositional []) arguments
  HelpRequested -> HelpRequested
  HelpText text -> HelpText text
  Parsed taken (Positional second) rest ->
    if taken
      then andThen (manyFirst first rest) (\_ others final ->
        Parsed True (applyPositional (mapPositional (:) (Positional second)) others) final)
      else Parsed False (Positional (manySecond second)) rest

manySecond :: ([String] -> Result a) -> [String] -> Result [a]
manySecond second arguments = case second arguments of
  Failed _ -> Parsed False [] arguments
  HelpRequested -> HelpRequested
  HelpText text -> HelpText text
  Parsed taken a rest ->
    if taken
      then andThen (manySecond second rest) (\_ others final -> Parsed True (a : others) final)
      else Parsed False [] rest

-- Just the value of the parser, or Nothing if it fails.
optional :: Parser a -> Parser (Maybe a)
optional (Parser items first) = Parser items (\arguments ->
  case first arguments of
    Failed _ -> Parsed False (purePositional Nothing) arguments
    HelpRequested -> HelpRequested
    HelpText text -> HelpText text
    Parsed taken (Positional second) rest -> Parsed taken (Positional (\remaining ->
      case second remaining of
        Failed _ -> Parsed False Nothing remaining
        HelpRequested -> HelpRequested
        HelpText text -> HelpText text
        Parsed otherTaken a final -> Parsed otherTaken (Just a) final)) rest)

-- How an option, flag, argument or command is described: its long names,
-- short names, help text, metavariable, default value, and subcommands.
-- The first type parameter is there only because the real package has it.
data Mod f a = Mod [String] [Char] String String (Maybe a) [Command a]

data Command a = Command String (ParserInfo a)

instance Semigroup (Mod f a) where
  (<>) (Mod longs shorts helpText meta def commands) (Mod longs2 shorts2 helpText2 meta2 def2 commands2) =
    Mod (longs ++ longs2) (shorts ++ shorts2) (helpText ++ helpText2) (meta ++ meta2) (firstJust def def2) (commands ++ commands2)

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just a) _ = Just a
firstJust Nothing other = other

noMod :: Mod f a
noMod = Mod [] [] "" "" Nothing []

long :: String -> Mod f a
long name = Mod [name] [] "" "" Nothing []

short :: Char -> Mod f a
short name = Mod [] [name] "" "" Nothing []

help :: String -> Mod f a
help text = Mod [] [] text "" Nothing []

metavar :: String -> Mod f a
metavar name = Mod [] [] "" name Nothing []

value :: a -> Mod f a
value a = Mod [] [] "" "" (Just a) []

-- Defaults are not shown in the help.
showDefaultWith :: (a -> String) -> Mod f a
showDefaultWith _ = noMod

-- Internal options are shown in the help like any other.
internal :: Mod f a
internal = noMod

command :: String -> ParserInfo a -> Mod f a
command name info = Mod [] [] "" "" Nothing [Command name info]

-- How the header and description of a parser are given.
data InfoMod a = InfoMod String String

instance Semigroup (InfoMod a) where
  (<>) (InfoMod headerText description) (InfoMod headerText2 description2) =
    InfoMod (headerText ++ headerText2) (description ++ description2)

fullDesc :: InfoMod a
fullDesc = InfoMod "" ""

header :: String -> InfoMod a
header text = InfoMod text ""

progDesc :: String -> InfoMod a
progDesc text = InfoMod "" text

info :: Parser a -> InfoMod a -> ParserInfo a
info parser (InfoMod headerText description) = ParserInfo parser headerText description

-- Looking for one option in the arguments.
data Found
  = NotFound
  | -- The option is there, but its value is not.
    NoValue
  | -- The value, and the arguments without the option.
    Found String [String]

-- Whether the argument is one of the names: --long or -s.
isOption :: [String] -> [Char] -> String -> Bool
isOption longs shorts argument =
  any (\name -> argument == "--" ++ name) longs || any (\name -> argument == ['-', name]) shorts

stripPrefix :: String -> String -> Maybe String
stripPrefix [] text = Just text
stripPrefix (_ : _) [] = Nothing
stripPrefix (a : prefix) (b : text) = if a == b then stripPrefix prefix text else Nothing

firstMatch :: [String] -> String -> Maybe String
firstMatch [] _ = Nothing
firstMatch (prefix : rest) argument = case stripPrefix prefix argument of
  Just text -> Just text
  Nothing -> firstMatch rest argument

-- The value in --long=VALUE or -sVALUE, if the argument is one of those.
attachedValue :: [String] -> [Char] -> String -> Maybe String
attachedValue longs shorts argument = case firstMatch (map (\name -> "--" ++ name ++ "=") longs) argument of
  Just text -> Just text
  Nothing -> case firstMatch (map (\name -> ['-', name]) shorts) argument of
    Just (c : text) -> Just (c : text)
    _ -> Nothing

takeOption :: [String] -> [Char] -> [String] -> Found
takeOption _ _ [] = NotFound
takeOption longs shorts (argument : rest) =
  if isOption longs shorts argument
    then case rest of
      [] -> NoValue
      text : final -> Found text final
    else case attachedValue longs shorts argument of
      Just text -> Found text rest
      Nothing -> case takeOption longs shorts rest of
        Found text final -> Found text (argument : final)
        other -> other

-- The arguments without the flag, if it is there.
takeFlag :: [String] -> [Char] -> [String] -> Maybe [String]
takeFlag _ _ [] = Nothing
takeFlag longs shorts (argument : rest) =
  if isOption longs shorts argument
    then Just rest
    else fmap (\final -> argument : final) (takeFlag longs shorts rest)

startsWithDash :: String -> Bool
startsWithDash ('-' : _ : _) = True
startsWithDash _ = False

-- The first argument that is not an option.
takePositional :: [String] -> Found
takePositional [] = NotFound
takePositional (argument : rest) =
  if startsWithDash argument
    then case takePositional rest of
      Found text final -> Found text (argument : final)
      other -> other
    else Found argument rest

-- How the option is written, for messages and help: -s, --long
optionNames :: [String] -> [Char] -> String
optionNames longs shorts = joinNames (map (\name -> ['-', name]) shorts ++ map (\name -> "--" ++ name) longs)

joinNames :: [String] -> String
joinNames [] = ""
joinNames [name] = name
joinNames (name : rest) = name ++ ", " ++ joinNames rest

optionItem :: [String] -> [Char] -> String -> String -> Item
optionItem longs shorts meta helpText = Item (optionNames longs shorts ++ withSpace meta) helpText

withSpace :: String -> String
withSpace "" = ""
withSpace text = ' ' : text

-- The first value if the flag is absent, the second if it is given.
flag :: a -> a -> Mod f a -> Parser a
flag absent given (Mod longs shorts helpText _ _ _) = Parser [optionItem longs shorts "" helpText] (\arguments ->
  case takeFlag longs shorts arguments of
    Nothing -> Parsed False (purePositional absent) arguments
    Just rest -> Parsed True (purePositional given) rest)

switch :: Mod f Bool -> Parser Bool
switch = flag False True

-- How the text of an option becomes its value.
newtype ReadM a = ReadM (String -> Either String a)

eitherReader :: (String -> Either String a) -> ReadM a
eitherReader = ReadM

option :: ReadM a -> Mod f a -> Parser a
option (ReadM reader) (Mod longs shorts helpText meta def _) = Parser [optionItem longs shorts meta helpText] (\arguments ->
  case takeOption longs shorts arguments of
    NotFound -> case def of
      Just a -> Parsed False (purePositional a) arguments
      Nothing -> Failed ("Missing: " ++ optionNames longs shorts ++ withSpace meta)
    NoValue -> Failed ("The option `" ++ optionNames longs shorts ++ "` expects an argument.")
    Found text rest -> case reader text of
      Left message -> Failed ("option " ++ optionNames longs shorts ++ ": " ++ message)
      Right a -> Parsed True (purePositional a) rest)

strOption :: Mod f String -> Parser String
strOption = option (ReadM Right)

strArgument :: Mod f String -> Parser String
strArgument (Mod _ _ helpText meta def _) = Parser [Item meta helpText] (\arguments ->
  Parsed False (Positional (\remaining -> case takePositional remaining of
    Found text rest -> Parsed True text rest
    _ -> case def of
      Just text -> Parsed False text remaining
      Nothing -> Failed ("Missing: " ++ meta))) arguments)

findCommand :: String -> [Command a] -> Maybe (ParserInfo a)
findCommand _ [] = Nothing
findCommand name (Command other info : rest) = if name == other then Just info else findCommand name rest

commandItem :: Command a -> Item
commandItem (Command name (ParserInfo _ _ description)) = Item name description

-- The first argument that is not an option names a command. Everything
-- after it belongs to that command.
subparser :: Mod f a -> Parser a
subparser (Mod _ _ _ _ _ commands) = Parser (map commandItem commands) (\arguments ->
  case takePositional arguments of
    Found name rest -> case findCommand name commands of
      Nothing -> Failed ("Invalid argument `" ++ name ++ "'")
      Just info -> andThen (runInfo info (afterCommand name arguments)) (\_ a _ ->
        Parsed True (purePositional a) (beforeCommand name arguments))
    _ -> Parsed False (Positional (\_ -> Failed "Missing: COMMAND")) arguments)

beforeCommand :: String -> [String] -> [String]
beforeCommand _ [] = []
beforeCommand name (argument : rest) = if argument == name then [] else argument : beforeCommand name rest

afterCommand :: String -> [String] -> [String]
afterCommand _ [] = []
afterCommand name (argument : rest) = if argument == name then rest else afterCommand name rest

-- --help or -h: print the help of the enclosing parser.
helper :: Parser (a -> a)
helper = Parser [Item "-h, --help" "Show this help text"] (\arguments ->
  case takeFlag ["help"] ['h'] arguments of
    Nothing -> Parsed False (purePositional id) arguments
    Just _ -> HelpRequested)

renderItem :: Item -> String
renderItem (Item name helpText) = "  " ++ name ++ "\n      " ++ helpText ++ "\n"

renderHelp :: String -> String -> [Item] -> String
renderHelp headerText description items =
  paragraph headerText ++ paragraph description ++ "Available options and commands:\n" ++ concatMap renderItem items

paragraph :: String -> String
paragraph "" = ""
paragraph text = text ++ "\n\n"

-- Runs both passes, and insists that no argument is left over.
runInfo :: ParserInfo a -> [String] -> Result a
runInfo (ParserInfo (Parser items first) headerText description) arguments =
  case andThen (first arguments) (\_ (Positional second) rest -> second rest) of
    HelpRequested -> HelpText (renderHelp headerText description items)
    Parsed taken a [] -> Parsed taken a []
    Parsed _ _ (extra : _) ->
      if startsWithDash extra
        then Failed ("Invalid option `" ++ extra ++ "'")
        else Failed ("Invalid argument `" ++ extra ++ "'")
    result -> result

execParser :: ParserInfo a -> IO a
execParser info = getArgs >>= (\arguments -> case runInfo info arguments of
  Parsed _ a _ -> pure a
  HelpText text -> putStr text >> exitSuccess
  HelpRequested -> exitSuccess
  Failed message -> hPutStrLn stderr message >> exitFailure)
