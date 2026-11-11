-- Writes src/unicode_table.h: what GHC's Data.Char says about every
-- character, as runs. Run it with the GHC whose answers the interpreter
-- should give:
--
--   runghc --ghc-arg=-package-env --ghc-arg=- scripts/gen-unicode.hs > src/unicode_table.h
module Main where

import Data.Char
import System.Info (compilerVersion)
import Data.Version (showVersion)

-- The places where the value changes, with the new value.
runs :: Eq a => (Char -> a) -> [(Int, a)]
runs f = go [(fromEnum c, f c) | c <- [minBound .. maxBound]]
  where
    go ((i, a) : rest) = (i, a) : go (dropWhile ((== a) . snd) rest)
    go [] = []

table :: String -> [(Int, Int)] -> [String]
table name entries =
  ["static const Run " ++ name ++ "[] = {"]
    ++ map (\row -> "    " ++ concatMap (\(i, a) -> "{" ++ show i ++ "," ++ show a ++ "},") row) (chunks 6 entries)
    ++ ["};", ""]
  where
    chunks n list = if null list then [] else take n list : chunks n (drop n list)

delta :: (Char -> Char) -> Char -> Int
delta f c = fromEnum (f c) - fromEnum c

main :: IO ()
main =
  putStr
    ( unlines
        ( [ "/* Made by scripts/gen-unicode.hs with GHC " ++ showVersion compilerVersion ++ ". Do not edit.",
            "   Each table lists where the value changes: from that character on, up",
            "   to the next entry, the value is the one given. */",
            ""
          ]
            ++ table "category_runs" (runs (fromEnum . generalCategory))
            ++ table "upper_runs" (runs (delta toUpper))
            ++ table "lower_runs" (runs (delta toLower))
            ++ table "title_runs" (runs (delta toTitle))
            ++ table "is_upper_runs" (runs (fromEnum . isUpper))
            ++ table "is_lower_runs" (runs (fromEnum . isLower))
        )
    )
