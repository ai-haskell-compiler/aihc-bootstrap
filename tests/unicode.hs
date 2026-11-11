module Main where

import Data.Char
import Data.List (foldl')

-- Every character below U+3000, and every 211th one after that: enough to
-- pass through all the scripts and the planes with characters in them.
sample :: [Char]
sample = map toEnum ([0 .. 0x2FFF] ++ [0x3000, 0x30D3 .. 0x10FFFF])

-- A number that depends on every answer and on its position.
checksum :: [Int] -> Int
checksum = foldl' (\total n -> (total * 31 + n) `mod` 1000000007) 7

count :: (Char -> Bool) -> Int
count test = length (filter test sample)

main :: IO ()
main = do
  print (length sample)
  print (checksum (map (fromEnum . generalCategory) sample))
  print (map (\category -> length (filter ((== category) . generalCategory) sample)) [minBound .. maxBound])
  print (map count [isUpper, isLower, isAlpha, isLetter, isAlphaNum, isDigit, isNumber, isMark])
  print (map count [isSpace, isControl, isPrint, isPunctuation, isSymbol, isSeparator, isAscii, isLatin1])
  print (checksum (map (fromEnum . isUpper) sample), checksum (map (fromEnum . isLower) sample), checksum (map (fromEnum . isAlphaNum) sample), checksum (map (fromEnum . isPrint) sample))
  print (checksum (map (fromEnum . toUpper) sample), checksum (map (fromEnum . toLower) sample), checksum (map (fromEnum . toTitle) sample))
  print (map generalCategory "aA1 _-(«»+$\n\173λЖ中ǅ٣Ⅷ\8232\57344\888")
  print (map toUpper "straße λόγος ǆ ÿ ı", map toLower "ÀÉÎ ΣΑΣ Ǆ İ", map toTitle "ǆ ß a")
  print (filter isSpace (map toEnum [0 .. 0x3100]) == " " || True, map fromEnum (filter isSpace (map toEnum [0 .. 0x3100])))
  print (words "a\160b\8195c d", isAlpha 'ª', isLower 'ª', isUpper 'Ⅷ', isAlphaNum '²', isPunctuation '¿', isSymbol '€', isMark '\769')
  print [minBound .. maxBound :: GeneralCategory]
