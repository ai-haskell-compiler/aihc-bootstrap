{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

data Colour = Red | Green | Blue
  deriving (Eq, Ord, Show, Enum, Bounded)

data Shape
  = Circle Colour Int
  | Square {side :: Int, fill :: Maybe Colour}
  | Group [Shape]
  deriving (Eq, Ord, Show)

data Pair a b = Pair a b
  deriving (Eq, Ord, Show, Bounded)

newtype Wrapper a = Wrapper a
  deriving (Eq, Ord, Show)

newtype Count = Count Int
  deriving stock (Show)
  deriving newtype (Eq, Ord, Num)

-- A field type with its own instance: the derived instance must use it.
newtype Loose = Loose Int

instance Eq Loose where
  (==) _ _ = True

instance Show Loose where
  show _ = "<loose>"

data Holder = Holder Loose Int
  deriving (Eq, Show)

main :: IO ()
main = do
  print [Red ..]
  print (Red == Red, Red == Blue, Red /= Green, compare Green Blue, Blue > Red)
  print (minBound :: Colour, maxBound :: Colour, succ Red, pred Blue, fromEnum Blue, toEnum 1 :: Colour)
  print [Blue, Green ..]
  print [Red, Blue ..]
  print (Circle Red (-3))
  print (Square {side = 2, fill = Just Blue})
  print (Group [Circle Green 1, Square 0 Nothing])
  print (Just (Circle Red 1), [Left 1, Right "two"] :: [Either Int String])
  print (Circle Red 1 == Circle Red 1, Circle Red 1 == Circle Red 2, Circle Blue 0 < Square 0 Nothing)
  print (compare (Group [Circle Red 1]) (Group [Circle Red 1, Circle Red 0]), Square 1 Nothing < Square 1 (Just Red))
  print (Pair 'x' (Wrapper True), Pair (Just (-1 :: Int)) ())
  print (minBound :: Pair Bool Colour, maxBound :: Pair Colour Char)
  print (Wrapper (Wrapper (3 :: Int)), Wrapper 1 < Wrapper (2 :: Int))
  print (Count 3 + Count 4, Count 2 == Count 2, Count 1 < 5)
  print (Holder (Loose 1) 2 == Holder (Loose 9) 2, Holder (Loose 1) 2 == Holder (Loose 1) 3, Holder (Loose 1) 2)
  print (maximum [Green, Blue, Red], [minBound .. maxBound :: Bool], (LT, EQ, GT))
