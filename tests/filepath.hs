module Main where

import System.FilePath

paths :: [FilePath]
paths =
  [ "", ".", "..", "/", "//", "///x", "a", "a/", "a//", "/a", "/a/", "a/b", "a/b/", "/a/b/c.hs", "a//b///c",
    "./a", "a/.", "a/./b", "./", "/.", "/./", "a/..", "../a/../b", "a/b/..", "x.tar.gz", "/d.e/f", "/d.e/f.g",
    ".bashrc", "dir/.hidden", "dir/.hidden.txt", "file.", "dir.d/", "a.b/c", "a b/c d.e f", "/usr/local/bin/ghc",
    "héllo/wörld.txt", "..a", "a..b", "/..", "./.", "././a", "a/./", "//a//b//"
  ]

table :: Show a => String -> (FilePath -> a) -> IO ()
table name f = do
  putStrLn name
  mapM_ (\path -> putStrLn ("  " ++ show path ++ " -> " ++ show (f path))) paths

main :: IO ()
main = do
  table "takeDirectory" takeDirectory
  table "takeFileName" takeFileName
  table "takeBaseName" takeBaseName
  table "takeExtension" takeExtension
  table "takeExtensions" takeExtensions
  table "dropExtension" dropExtension
  table "dropExtensions" dropExtensions
  table "splitFileName" splitFileName
  table "splitExtension" splitExtension
  table "splitPath" splitPath
  table "splitDirectories" splitDirectories
  table "joinPath . splitPath" (joinPath . splitPath)
  table "normalise" normalise
  table "isRelative" isRelative
  table "isAbsolute" isAbsolute
  table "hasExtension" hasExtension
  table "hasTrailingPathSeparator" hasTrailingPathSeparator
  table "addTrailingPathSeparator" addTrailingPathSeparator
  table "dropTrailingPathSeparator" dropTrailingPathSeparator
  table "<.> hs" (<.> "hs")
  table "<.> .hs" (<.> ".hs")
  table "<.> empty" (<.> "")
  table "replaceExtension o" (`replaceExtension` "o")
  table "-<.> .tar.gz" (-<.> ".tar.gz")
  table "replaceFileName new" (`replaceFileName` "new")
  table "replaceBaseName new" (`replaceBaseName` "new")
  table "replaceDirectory new" (`replaceDirectory` "new")
  table "dir </>" ("dir" </>)
  table "dir/ </>" ("dir/" </>)
  table "empty </>" ("" </>)
  table "</> name" (</> "name")
  table "</> /abs" (</> "/abs")
  table "makeRelative /a" (makeRelative "/a")
  table "makeRelative a" (makeRelative "a")
  table "makeRelative a/b/" (makeRelative "a/b/")
  table "makeRelative ./a" (makeRelative "./a")
  table "makeRelative empty" (makeRelative "")
  table "makeRelative to /a/b/c.hs" (`makeRelative` "/a/b/c.hs")
  table "equalFilePath a/b" (equalFilePath "a/b")
  print (joinPath ["a", "b", "c"], joinPath ["/", "a/", "/b", "c"], joinPath [])
  print (splitSearchPath "/bin:/usr/bin::.:/x/", splitSearchPath "")
  print ("a" </> "b" </> "c" <.> "d", "a" <.> "b" <.> "c")
  print (pathSeparator, extSeparator, searchPathSeparator)
