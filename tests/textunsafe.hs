module Main where

import qualified Data.Text as T
import qualified Data.Text.Unsafe as TU

samples :: [T.Text]
samples = map T.pack ["", "a", "hello world", "héllo", "naïve café", "日本語 text", "a\128512b", "\955x.x", "tab\tnew\nline"]

-- Every character with its byte offset and width, walking as a lexer does.
walk :: T.Text -> [(Int, Char, Int)]
walk text = go 0
  where
    end = TU.lengthWord8 text
    go i
      | i >= end = []
      | otherwise =
          let TU.Iter c d = TU.iter text i
           in (i, c, d) : go (i + d)

main :: IO ()
main = do
  mapM_ (print . TU.lengthWord8) samples
  mapM_ (print . walk) samples
  mapM_ (\t -> print [T.unpack (TU.takeWord8 i t) | (i, _, _) <- walk t]) samples
  mapM_ (\t -> print [T.unpack (TU.dropWord8 i t) | (i, _, _) <- walk t]) samples
  mapM_ (\t -> print (TU.takeWord8 (TU.lengthWord8 t) t == t, T.null (TU.dropWord8 (TU.lengthWord8 t) t))) samples
  print (TU.lengthWord8 (T.takeWhile (/= ' ') (T.pack "日本語 text")))
