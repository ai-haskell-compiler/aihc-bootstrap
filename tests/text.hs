{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

sample :: Text
sample = "  Hello, wide world!  "

main :: IO ()
main = do
  print sample
  print (T.unpack sample, T.pack "packed", T.length sample, T.null sample, T.null "", T.empty, T.singleton 'x')
  print (T.strip sample, T.stripStart sample, T.stripEnd sample, T.dropAround (== ' ') sample)
  print (T.toUpper "mixed Case 12", T.toLower "MIXED Case 12", T.reverse "abc", T.map succ "abc")
  print (T.take 3 "abcdef", T.drop 3 "abcdef", T.takeEnd 2 "abcdef", T.dropEnd 2 "abcdef", T.splitAt 2 "abcdef", T.take 9 "ab", T.drop (-1) "ab")
  print (T.takeWhile (/= 'c') "abcdef", T.dropWhile (/= 'c') "abcdef", T.takeWhileEnd (/= 'c') "abcdef", T.dropWhileEnd (/= 'c') "abcdef")
  print (T.span (< 'c') "abcab", T.break (== 'c') "abcab", T.filter (/= 'a') "banana", T.findIndex (== 'n') "banana", T.elem 'z' "banana")
  print (T.uncons "abc", T.uncons "", T.cons 'x' "yz", T.head "abc", T.last "abc", T.tail "abc", T.any (== 'b') "abc", T.all (== 'b') "abc")
  print (T.splitOn "," "a,b,,c", T.splitOn "::" "a::b:c", T.splitOn "," "", T.splitOn "ab" "ababab")
  print (T.breakOn "::" "key::value::more", T.breakOn "x" "none", T.replace "an" "AN" "banana", T.count "an" "banana")
  print (T.intercalate ", " ["a", "b", "c"], T.concat ["x", "y"], T.concatMap (\c -> T.pack [c, c]) "abc", T.replicate 3 "ab")
  print (T.isPrefixOf "ab" "abc", T.isSuffixOf "bc" "abc", T.isInfixOf "x" "abc", T.isInfixOf "" "abc")
  print (T.stripPrefix "ab" "abc", T.stripPrefix "x" "abc", T.stripSuffix "bc" "abc", T.stripSuffix "x" "abc")
  print (T.lines "one\ntwo\n\nfour", T.unlines ["a", "b"], T.words "  two   words\there ", T.unwords ["a", "b"])
  print (T.foldl' (\n c -> n * 10 + fromEnum c - 48) 0 "1234", T.compareLength "abc" 2, T.compareLength "abc" 3, T.compareLength "abc" 4)
  print ("abc" < ("abd" :: Text), compare ("b" :: Text) "ab", ("a" :: Text) == "a", "x" <> "y" <> (mempty :: Text), maximum ["pear", "apple" :: Text])
  print (T.pack (show (T.pack "quote\"d")), T.unpack "tab\there", T.length "λ→x")
  TIO.putStrLn (T.unwords ["printed", "as", "text:", "λ"])
  TIO.putStr "no newline"
  TIO.putStrLn ""
  print (map describe ["", "a", "abc"])
  print ('x' :< "yz", "xy" :> 'z', Empty == T.empty)
  where
    describe :: Text -> String
    describe text = case text of
      Empty -> "empty"
      c :< Empty -> "one: " ++ [c]
      c :< (middle :> d) -> [c] ++ show (T.length middle) ++ [d]
