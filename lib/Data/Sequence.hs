module Data.Sequence where

-- A sequence as a list, with only what hsc2hs uses of the package
-- containers. Adding at the end takes time in the length, which is fine
-- for the short sequences hsc2hs makes.

import qualified Data.List as List

newtype Seq a = Seq [a]

instance Show a => Show (Seq a) where
  showsPrec d (Seq items) = showParen (d > 10) (showString "fromList " . shows items)

instance Eq a => Eq (Seq a) where
  (==) (Seq a) (Seq b) = a == b

instance Functor Seq where
  fmap f (Seq items) = Seq (map f items)

instance Foldable Seq where
  foldr f start (Seq items) = List.foldr f start items

data ViewL a = EmptyL | a :< Seq a

infixr 5 :<
infixl 5 |>

empty :: Seq a
empty = Seq []

singleton :: a -> Seq a
singleton a = Seq [a]

fromList :: [a] -> Seq a
fromList = Seq

(|>) :: Seq a -> a -> Seq a
(|>) (Seq items) a = Seq (items ++ [a])

viewl :: Seq a -> ViewL a
viewl (Seq items) = case items of
  [] -> EmptyL
  a : rest -> a :< Seq rest
