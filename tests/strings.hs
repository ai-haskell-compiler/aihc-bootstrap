module Main where

classify :: String -> String
classify "" = "empty"
classify "λ" = "lambda"
classify ('\t' : _) = "tab first"
classify _ = "other"

main :: IO ()
main = do
  putStrLn "tab:\t| quote:\" | backslash:\\ | apostrophe:\'"
  putStrLn "decimal:\955 hex:\x3bb octal:\o1673 literal:λ"
  putStrLn "names:\SOH\&1 \ESC[0m \DEL| control:\^A|"
  putStrLn "a gap \
           \joins lines"
  putStrLn ['a', '\'', '\\', '\955', 'λ']
  putStrLn (classify "")
  putStrLn (classify "λ")
  putStrLn (classify "\tx")
  putStrLn (classify "x")
