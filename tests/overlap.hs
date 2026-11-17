{-# LANGUAGE FlexibleInstances #-}

module Main where

-- Of two instances that are both for a type, the one for fewer types is
-- meant, wherever the two are written.

class Describe a where
  describe :: a -> String

instance {-# OVERLAPPABLE #-} Describe a => Describe [a] where
  describe list = "[" ++ concatMap describe list ++ "]"

instance {-# OVERLAPPING #-} Describe String where
  describe string = "string " ++ string

instance Describe Bool where
  describe b = if b then "yes" else "no"

instance Describe Char where
  describe c = [c]

instance {-# OVERLAPPING #-} Describe (Maybe Bool) where
  describe _ = "maybe a bool"

instance {-# OVERLAPPABLE #-} Describe a => Describe (Maybe a) where
  describe = maybe "nothing" describe

main :: IO ()
main = do
  putStrLn (describe "abc")
  putStrLn (describe [True, False])
  putStrLn (describe ["ab", "c"])
  putStrLn (describe (Just True))
  putStrLn (describe (Just 'x'))
  putStrLn (describe [Just "s", Nothing])
