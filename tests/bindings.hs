module Main where

data Colour = Red | Green

class Render a where
  render1 :: a -> String

instance Render Colour where
  render1 Red = "red"
  render1 Green = "green"

-- where over a clause, with a local function used at two types.
pairs :: Colour -> String
pairs colour = render (twice colour) ++ " " ++ render (twice "s")
  where
    twice x = (x, x)
    render (a, b) = render1 a ++ "+" ++ render1 b

instance Render Char where
  render1 c = [c]

instance Render a => Render [a] where
  render1 [] = ""
  render1 (x : rest) = render1 x ++ render1 rest

-- A local function that calls itself, without a signature.
count :: [a] -> String
count list = go list ""
  where
    go [] marks = marks
    go (_ : rest) marks = go rest ('|' : marks)

-- A local function with a signature, polymorphic in its own variable.
both :: Colour -> String
both colour =
  let pick :: a -> a -> a
      pick first _ = first
   in render1 (pick colour Green) ++ pick "!" "?"

-- Pattern bindings are lazy, and may refer to each other.
swap :: (a, b) -> (b, a)
swap pair = (second, first)
  where
    (first, second) = pair

ones :: String
ones = take3 stream
  where
    stream = '1' : stream
    take3 (a : b : c : _) = [a, b, c]

-- where sees the arguments, and nests.
describe :: Colour -> String -> String
describe colour suffix = outer
  where
    outer = inner ++ suffix
      where
        inner = render1 colour

-- A literal in a local binding takes its type from its use.
data Size = Size Int

sized :: Size
sized = Size n
  where
    n = 3

main :: IO ()
main = do
  putStrLn (pairs Red)
  putStrLn (count [Red, Green, Red])
  putStrLn (both Red)
  let (a, b) = swap ("left", Green)
      shout text = text ++ "!"
  putStrLn (shout (render1 a) ++ shout b)
  putStrLn ones
  let twice = describe Green " twice"
   in putStrLn twice
  case sized of
    Size _ -> putStrLn (describe Red " done")
