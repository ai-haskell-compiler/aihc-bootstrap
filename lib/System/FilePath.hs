module System.FilePath where

-- The POSIX half of the package filepath, with its definitions. What that
-- package calls the drive is, on POSIX, the slashes a path starts with.

import Data.List (dropWhileEnd)

infixr 7 <.>

infixr 5 </>

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'

extSeparator :: Char
extSeparator = '.'

isExtSeparator :: Char -> Bool
isExtSeparator c = c == '.'

searchPathSeparator :: Char
searchPathSeparator = ':'

-- The pieces of a PATH variable. An empty piece is the current directory.
splitSearchPath :: String -> [FilePath]
splitSearchPath string = case break (== searchPathSeparator) string of
  (piece, []) -> name piece
  (piece, _ : rest) -> name piece ++ splitSearchPath rest
  where
    name piece = if null piece then ["."] else [piece]

-- The longest end of the list that has no element passing the test, and
-- what is before it.
breakEnd :: (a -> Bool) -> [a] -> ([a], [a])
breakEnd test list = case break test (reverse list) of
  (end, start) -> (reverse start, reverse end)

-- ---- Drives ----

splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive = span isPathSeparator

takeDrive :: FilePath -> FilePath
takeDrive path = fst (splitDrive path)

dropDrive :: FilePath -> FilePath
dropDrive path = snd (splitDrive path)

hasDrive :: FilePath -> Bool
hasDrive path = not (null (takeDrive path))

isDrive :: FilePath -> Bool
isDrive path = not (null path) && null (dropDrive path)

joinDrive :: FilePath -> FilePath -> FilePath
joinDrive = combineAlways

-- ---- Extensions ----

splitExtension :: FilePath -> (String, String)
splitExtension path = case breakEnd isExtSeparator file of
  ([], _) -> (path, "")
  (nameDot, extension) -> (directory ++ init nameDot, extSeparator : extension)
  where
    (directory, file) = splitFileName_ path

takeExtension :: FilePath -> String
takeExtension path = snd (splitExtension path)

dropExtension :: FilePath -> FilePath
dropExtension path = fst (splitExtension path)

replaceExtension :: FilePath -> String -> FilePath
replaceExtension path extension = dropExtension path <.> extension

(-<.>) :: FilePath -> String -> FilePath
(-<.>) = replaceExtension

addExtension :: FilePath -> String -> FilePath
addExtension file extension = case extension of
  [] -> file
  c : _ -> joinDrive drive (if isExtSeparator c then rest ++ extension else rest ++ [extSeparator] ++ extension)
  where
    (drive, rest) = splitDrive file

(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

hasExtension :: FilePath -> Bool
hasExtension path = any isExtSeparator (takeFileName path)

splitExtensions :: FilePath -> (FilePath, String)
splitExtensions path = case break isExtSeparator file of
  (name, extensions) -> (directory ++ name, extensions)
  where
    (directory, file) = splitFileName_ path

takeExtensions :: FilePath -> String
takeExtensions path = snd (splitExtensions path)

dropExtensions :: FilePath -> FilePath
dropExtensions path = fst (splitExtensions path)

-- ---- File names and directories ----

splitFileName :: FilePath -> (String, String)
splitFileName path = (if null directory then "./" else directory, name)
  where
    (directory, name) = splitFileName_ path

-- As splitFileName, but the directory of a bare file name is empty.
splitFileName_ :: FilePath -> (String, String)
splitFileName_ path = (drive ++ directory, file)
  where
    (drive, rest) = splitDrive path
    (directory, file) = breakEnd isPathSeparator rest

takeFileName :: FilePath -> FilePath
takeFileName path = snd (splitFileName path)

dropFileName :: FilePath -> FilePath
dropFileName path = fst (splitFileName path)

replaceFileName :: FilePath -> String -> FilePath
replaceFileName path name = fst (splitFileName_ path) </> name

takeBaseName :: FilePath -> String
takeBaseName path = dropExtension (takeFileName path)

replaceBaseName :: FilePath -> String -> FilePath
replaceBaseName path name = combineAlways directory (name <.> extension)
  where
    (directory, file) = splitFileName_ path
    extension = takeExtension file

takeDirectory :: FilePath -> FilePath
takeDirectory path = dropTrailingPathSeparator (dropFileName path)

replaceDirectory :: FilePath -> String -> FilePath
replaceDirectory path directory = combineAlways directory (takeFileName path)

hasTrailingPathSeparator :: FilePath -> Bool
hasTrailingPathSeparator path = not (null path) && isPathSeparator (last path)

addTrailingPathSeparator :: FilePath -> FilePath
addTrailingPathSeparator path =
  if hasTrailingPathSeparator path then path else path ++ [pathSeparator]

dropTrailingPathSeparator :: FilePath -> FilePath
dropTrailingPathSeparator path =
  if hasTrailingPathSeparator path && not (isDrive path)
    then
      let shorter = dropWhileEnd isPathSeparator path
       in if null shorter then [last path] else shorter
    else path

-- ---- Joining and splitting ----

combine :: FilePath -> FilePath -> FilePath
combine a b
  | not (null b) && isPathSeparator (head b) = b
  | otherwise = combineAlways a b

combineAlways :: FilePath -> FilePath -> FilePath
combineAlways a b
  | null a = b
  | null b = a
  | hasTrailingPathSeparator a = a ++ b
  | otherwise = a ++ [pathSeparator] ++ b

(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

-- The pieces keep the slashes that follow them.
splitPath :: FilePath -> [FilePath]
splitPath path = [drive | not (null drive)] ++ pieces rest
  where
    (drive, rest) = splitDrive path
    pieces [] = []
    pieces string = case break isPathSeparator string of
      (name, afterName) -> case span isPathSeparator afterName of
        (slashes, afterSlashes) -> (name ++ slashes) : pieces afterSlashes

splitDirectories :: FilePath -> [FilePath]
splitDirectories path = map dropTrailingPathSeparator (splitPath path)

joinPath :: [FilePath] -> FilePath
joinPath = foldr combine ""

-- ---- Comparing and simplifying ----

equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath a b = simple a == simple b
  where
    simple path = dropTrailingPathSeparator (normalise path)

-- The path from the first argument to the second, if the second is inside
-- the first; otherwise the second argument.
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative root path
  | equalFilePath root path = "."
  | takeDrive root /= takeDrive path = path
  | otherwise = inside (dropDrive root) (dropDrive path)
  where
    inside x y
      | null x = dropWhile isPathSeparator y
      | otherwise = case (first x, first y) of
          ((x1, x2), (y1, y2)) -> if equalFilePath x1 y1 then inside x2 y2 else path
    -- The first name of the path, and what follows it.
    first x = case break isPathSeparator (dropWhile isPathSeparator x) of
      (a, b) -> (dropWhile isPathSeparator a, dropWhile isPathSeparator b)

-- Removes "." names and repeated slashes. It does not remove "..".
normalise :: FilePath -> FilePath
normalise path = result ++ [pathSeparator | addPathSeparator]
  where
    (drive, rest) = splitDrive path
    result = joinDrive' (if null drive then "" else [pathSeparator]) (simplify rest)
    joinDrive' d p = if null d && null p then "." else joinDrive d p
    addPathSeparator = isDirPath rest && not (hasTrailingPathSeparator result)
    isDirPath xs =
      hasTrailingPathSeparator xs
        || not (null xs) && last xs == '.' && hasTrailingPathSeparator (init xs)
    simplify string = joinPath (filter ("." /=) (splitDirectories string))

isRelative :: FilePath -> Bool
isRelative path = not (hasDrive path)

isAbsolute :: FilePath -> Bool
isAbsolute path = not (isRelative path)

isValid :: FilePath -> Bool
isValid path = not (null path) && notElem '\0' path
