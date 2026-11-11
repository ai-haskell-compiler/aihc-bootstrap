module Main where

import Data.Char (digitToInt)
import Data.Int (Int8)
import Data.Word (Word8)
import Numeric (readDec, readHex, readInt, readOct, showHex, showOct)
import Text.Read (readEither, readMaybe)

ints :: [String]
ints = ["42", " 42 ", "-7", "- 7", "+7", "0x1F", "0X1f", "0o17", "007", "(5)", "((-5))", "( 5", "5)", "", " ", "abc", "4 2", "42abc", "1.5", "1e3", "1e", "1.", "0x", "0xg", "--1", "12345678901"]

main :: IO ()
main = do
  print (map (\s -> readMaybe s :: Maybe Int) ints)
  print (map (\s -> reads s :: [(Integer, String)]) ints)
  print (read "255" :: Word8, read "256" :: Word8, read "-1" :: Word8, read "-129" :: Int8, read " 12 " :: Integer)
  print (readEither "12" :: Either String Int, readEither "x" :: Either String Int)
  print (map (\s -> reads s :: [(Char, String)]) ["'a'", " 'b' rest", "'\\n'", "'\\''", "'\\x41'", "'\\65'", "''", "'ab'", "a", "('c')"])
  print (map (\s -> reads s :: [(String, String)]) ["\"abc\" rest", "\"\"", " \"a\\nb\\\"c\\\\\"", "\"\\SOH\\SO\\&H\\1234\\&5\"", "\"a\\   \\b\"", "\"open", "['a','b']", "abc", "(\"p\")"])
  print (map (\s -> readMaybe s :: Maybe [Int]) ["[1,2,3]", "[]", " [ 1 , -2 ] ", "[1,]", "[1 2]", "1", "[[1]]"])
  print (readMaybe "[[1,2],[],[3]]" :: Maybe [[Int]], readMaybe "[\"a\",\"b\"]" :: Maybe [String])
  print (map (\s -> readMaybe s :: Maybe Bool) ["True", "False", " True ", "true", "Truex", "(False)"])
  print (readMaybe "()" :: Maybe (), readMaybe "( )" :: Maybe (), readMaybe "(1,'c')" :: Maybe (Int, Char), readMaybe " ( 1 , True ) " :: Maybe (Int, Bool))
  print (map (\s -> readMaybe s :: Maybe (Maybe Int)) ["Nothing", "Just 5", "Just (-5)", "Just -5", "(Just 5)", "Just Nothing", "Justice"])
  print (readMaybe "Just (Just 3)" :: Maybe (Maybe (Maybe Int)), readMaybe "[Just 1,Nothing]" :: Maybe [Maybe Int])
  putStrLn (showHex (255 :: Int) "" ++ " " ++ showHex (0 :: Int) "" ++ " " ++ showHex (3735928559 :: Integer) "!" ++ " " ++ showOct (64 :: Word8) "")
  print (map (\n -> showHex n "") [1 :: Int, 15, 16, 4095, 65536])
  print (readHex "ff rest" :: [(Int, String)], readHex "FFg" :: [(Int, String)], readHex "xyz" :: [(Int, String)], readHex "" :: [(Int, String)])
  print (readOct "178" :: [(Int, String)], readDec "12x" :: [(Integer, String)], readInt 2 (`elem` "01") digitToInt "10112" :: [(Int, String)])
