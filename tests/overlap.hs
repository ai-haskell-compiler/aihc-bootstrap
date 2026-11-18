{-# LANGUAGE FlexibleInstances #-}

module Main where

-- Overlapping instances: the most specific one that matches is used, as
-- AIHC's Cbor instances for String and [a] need.

class Describe a where
  describe :: a -> String

instance Describe Char where
  describe c = [c]

instance Describe Int where
  describe n = "int " ++ show n

instance {-# OVERLAPPING #-} Describe String where
  describe s = "text " ++ show s

instance {-# OVERLAPPABLE #-} Describe a => Describe [a] where
  describe xs = "list of " ++ show (length xs) ++ ": " ++ concatMap describe xs

instance {-# OVERLAPPABLE #-} Describe a => Describe (Maybe a) where
  describe = maybe "nothing" describe

instance {-# OVERLAPPING #-} Describe (Maybe Int) where
  describe _ = "maybe an int"

main :: IO ()
main = do
  putStrLn (describe "abc")
  putStrLn (describe [1, 2 :: Int])
  putStrLn (describe ["ab", "c"])
  putStrLn (describe (Just 'x'))
  putStrLn (describe (Just (3 :: Int)))
  putStrLn (describe [Just "s"])
