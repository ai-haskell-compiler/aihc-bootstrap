module Data.String where

-- With OverloadedStrings, a string literal is fromString applied to a String.
class IsString a where
  fromString :: String -> a

-- A literal that is a list is a list of characters, whatever else is known
-- about the list. GHC says the same with the instance
-- (a ~ Char) => IsString [a].
instance IsString [a] where
  fromString = unsafeCoerce

foreign import prim "unsafeCoerce" unsafeCoerce :: a -> b
