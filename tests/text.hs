{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

import Data.Text (Text, pattern Empty, pattern (:<), pattern (:>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Unsafe as TU

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
  print (T.breakOnEnd "::" "key::value::more", T.breakOnEnd "x" "none", T.breakOnEnd "-" "aihc-base-4.21", T.breakOnEnd "ab" "ab")
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
  -- Characters of one to four bytes. An offset counts bytes.
  let mixed = "aé€😀z" :: Text
      iterAt i = case TU.iter mixed i of TU.Iter c d -> (c, d)
  print (T.length mixed, TU.lengthWord8 mixed, map iterAt [0, 1, 3, 6, 10])
  print (T.take 2 mixed, T.drop 3 mixed, T.splitAt 4 mixed, T.take 9 mixed, T.drop 9 mixed, T.take (-1) mixed)
  print (TU.takeWord8 3 mixed, TU.dropWord8 6 mixed, TU.unsafeHead (TU.dropWord8 1 mixed), TU.unsafeTail (TU.dropWord8 3 mixed))
  print (T.unsnoc mixed, T.last mixed, T.init mixed, T.index mixed 3, T.reverse mixed, T.unsnoc "")
  print (T.dropWhileEnd (> 'm') mixed, T.takeWhileEnd (> 'm') mixed, T.span (< 'z') mixed, T.dropWhile (/= '€') mixed)
  print (T.breakOn "€" mixed, T.breakOnEnd "é" (mixed <> mixed), T.splitOn "😀" (mixed <> mixed), T.isInfixOf "€😀" mixed)
  print (T.stripSuffix "😀z" mixed, T.stripPrefix "aé" mixed, T.isSuffixOf "z" mixed, T.isPrefixOf "é" mixed)
  print (compare ("é" :: Text) "z", compare ("😀" :: Text) "€", ("a" :: Text) < "ab", maximum ["b", "é", "a" :: Text])
  print (T.lines "one\n\nthree\n", T.lines "no newline", T.lines "", T.words "  two  words\there ", T.unlines ["a", "b"], T.unwords ["a", "b"])
  print (T.split (== ',') "a,,b,", T.split (== ',') "", T.any (== '€') mixed, T.all (< '€') mixed, T.find (> 'x') mixed, T.elem 'q' mixed)
  print (T.takeEnd 2 mixed, T.dropEnd 2 mixed, T.strip "  é  ", T.cons 'é' "x", T.snoc "x" '€', T.replicate 2 "é", T.concatMap (\c -> T.pack [c, c]) "aé")
  where
    describe :: Text -> String
    describe text = case text of
      Empty -> "empty"
      c :< Empty -> "one: " ++ [c]
      c :< (middle :> d) -> [c] ++ show (T.length middle) ++ [d]
