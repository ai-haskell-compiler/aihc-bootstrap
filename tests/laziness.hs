module Main where

-- What must not be evaluated: a pattern that is a variable or _ does not
-- look at the value it is matched with.

newtype Wrapped = Wrapped Int

data Pair = Pair Int Int

ignore :: a -> Int
ignore _ = 1

second :: a -> b -> b
second _ b = b

viaCase :: Int -> Int
viaCase x = case x of
  _ -> 2

named :: Int -> Int
named x = case x of
  y -> const 3 y

unwrap :: Wrapped -> Int
unwrap w = case w of
  Wrapped _ -> 4

-- The guard fails without looking at the value; the second alternative
-- matches without looking at it either.
guarded :: Bool -> Int -> Int
guarded flag x = case x of
  _ | flag -> 5
  _ -> 6

fields :: Pair -> Int
fields (Pair a _) = a

lazyLet :: Int
lazyLet =
  let (a, b) = undefined :: (Int, Int)
      c = a + b
   in const 7 c

main :: IO ()
main = do
  print (ignore (undefined :: Int), second (undefined :: Char) 'x')
  print (const 'a' (undefined :: Int), fst ('b', undefined :: Int))
  print (viaCase undefined, named undefined, unwrap undefined)
  print (guarded True undefined, guarded False undefined)
  print (fields (Pair 8 undefined), lazyLet)
  print (length [undefined, undefined :: Int], take 2 (map (const 'z') [undefined, undefined, undefined :: Int]))
  print ((\_ -> 9) (undefined :: Int), (\x -> 10) (undefined :: Int), (\(Wrapped _) -> 11) undefined)
