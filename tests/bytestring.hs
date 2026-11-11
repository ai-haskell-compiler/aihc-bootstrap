{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  let bytes = BS.pack [104, 101, 108, 108, 111, 0, 255, 10]
  print bytes
  print (BS.unpack bytes, BS.length bytes, BS.null bytes, BS.null BS.empty, BS.index bytes 6, BS.head bytes, BS.last bytes)
  print (BS.take 2 bytes, BS.drop 6 bytes, BS.take 99 bytes, BS.drop 99 bytes, BS.take (-1) bytes, BS.splitAt 5 bytes)
  print (BS.singleton 65, BS.replicate 3 66, BS.cons 67 "ab", BS.snoc "ab" 68, BS.concat ["ab", "", "cd"], "ab" <> ("cd" :: BS.ByteString))
  print (BS.uncons "ab", BS.uncons "", BS.init "abc", BS.map (+ 1) "abc", BS.filter (/= 98) "abcb", BS.foldl' (\n b -> n + fromIntegral b) (0 :: Int) "abc")
  print (BS.takeWhile (< 100) "abcdef", BS.span (< 100) "abcdef", BS.split 44 "a,b,,c", BS.split 44 "", BS.split 44 ",")
  print (BS.isInfixOf "cd" "abcdef", BS.isInfixOf "x" "abcdef", BS.breakSubstring "cd" "abcdef", BS.breakSubstring "x" "abc", BS.elemIndex 99 "abc", BS.elemIndex 0 "abc")
  print ("abc" == ("abc" :: BS.ByteString), compare ("abd" :: BS.ByteString) "abc", compare ("ab" :: BS.ByteString) "abc", ("b" :: BS.ByteString) > "abc")
  print (BS8.pack "char8 \955", BS8.unpack "plain", BS8.uncons "xy", BS8.cons 'a' "b", BS8.snoc "a" 'b', BS8.singleton 'z')
  print (BS8.lines "one\ntwo\n\nfour\n", BS8.words "  two   words\there ", BS8.unwords ["a", "b"], BS8.intercalate ", " ["x", "y"])
  print (BS8.readInt "123 rest", BS8.readInt "-45", BS8.readInt "x", BS8.readInteger "+7!", BS8.split ',' "a,b", BS8.splitWith (== ' ') "a b  c")
  print (BS8.dropWhile (== ' ') "  x ", BS8.dropWhileEnd (== ' ') "  x ", BS8.takeWhileEnd (/= '/') "dir/file", BS8.break (== '=') "k=v", BS8.elemIndexEnd '/' "a/b/c")
  print (BS8.isPrefixOf "ab" "abc", BS8.isSuffixOf "bc" "abc", BS8.isSuffixOf "abcd" "bcd", BS8.all (/= 'x') "abc", BS8.concatMap (BS8.replicate 2) "ab")
  let text = T.pack "utf-8: \955 \8594 \128512"
  print (TE.encodeUtf8 text, BS.length (TE.encodeUtf8 text), TE.decodeUtf8 (TE.encodeUtf8 text) == text)
  print (either (const "invalid") T.unpack (TE.decodeUtf8' (BS.pack [104, 105])), either (const "invalid") T.unpack (TE.decodeUtf8' (BS.pack [255, 104])))
  print (BL.fromStrict bytes == BL.fromChunks [BS.take 3 bytes, BS.drop 3 bytes], BL.length (BL.fromStrict bytes), BL.toChunks (BL.fromStrict "ab"), BL.toStrict (BL.take 2 (BL.fromStrict "abc")))
  -- Files: bytes and text come back as they were written.
  BS.writeFile "build/test-bytes.bin" bytes
  back <- BS.readFile "build/test-bytes.bin"
  print (back == bytes, BS.length back)
  TIO.writeFile "build/test-text.txt" text
  textBack <- TIO.readFile "build/test-text.txt"
  rawBack <- BS.readFile "build/test-text.txt"
  print (textBack == text, rawBack == TE.encodeUtf8 text)
  writeFile "build/test-string.txt" "line one\nline two\n"
  appendFile "build/test-string.txt" "line three\n"
  stringBack <- readFile "build/test-string.txt"
  print (lines stringBack)
  BS8.putStrLn "bytes to stdout"
