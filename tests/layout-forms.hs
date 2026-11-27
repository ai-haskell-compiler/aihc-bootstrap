-- Forms of layout and declarations that hsc2hs uses.

infixr 5 :<

-- A constructor between two types, the right one applied.
data View a = Empty | a :< Maybe a

-- A record whose closing brace is in the first column.
data Config = Config {
    name :: String,
    size :: Int
}

-- Clauses on one line, separated by semicolons.
isSymbol :: Char -> Bool
isSymbol '!' = True; isSymbol '#' = True
isSymbol _ = False

-- An `in` in the column of the bindings of its let.
pairs :: String -> [(String, String)]
pairs text = let
    this = case break (== '=') text of
        (left, []) -> (left, "")
        (left, _ : right) -> (left, right)
    in [this]

describe :: View Int -> String
describe Empty = "empty"
describe (a :< b) = show a ++ " then " ++ show b

main :: IO ()
main = do
  putStrLn (describe (1 :< Just 2))
  putStrLn (describe Empty)
  let c = Config {name = "c", size = 3}
  putStrLn (name c ++ show (size c))
  print (map isSymbol "!#a")
  print (pairs "x=1")
  print (pairs "y")
