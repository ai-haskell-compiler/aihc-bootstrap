module Data.List.NonEmpty where

-- A list with at least one element.

import Prelude hiding (filter, head, init, last, length, map, tail, zipWith)
import qualified Data.List as List
import qualified Prelude as P

infixr 5 :|

data NonEmpty a = a :| [a]
  deriving (Eq, Ord, Show)

instance Functor NonEmpty where
  fmap = map

instance Foldable NonEmpty where
  foldr f end (first :| rest) = f first (P.foldr f end rest)

instance Traversable NonEmpty where
  traverse f (first :| rest) = liftA2 (:|) (f first) (traverse f rest)

instance Semigroup (NonEmpty a) where
  (<>) (first :| rest) other = first :| (rest ++ toList other)

toList :: NonEmpty a -> [a]
toList (first :| rest) = first : rest

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty [] = Nothing
nonEmpty (first : rest) = Just (first :| rest)

fromList :: [a] -> NonEmpty a
fromList (first : rest) = first :| rest
fromList [] = error "NonEmpty.fromList: empty list"

singleton :: a -> NonEmpty a
singleton a = a :| []

head :: NonEmpty a -> a
head (first :| _) = first

tail :: NonEmpty a -> [a]
tail (_ :| rest) = rest

last :: NonEmpty a -> a
last (first :| rest) = P.last (first : rest)

init :: NonEmpty a -> [a]
init (first :| rest) = P.init (first : rest)

length :: NonEmpty a -> Int
length (_ :| rest) = 1 + P.length rest

cons :: a -> NonEmpty a -> NonEmpty a
cons a (first :| rest) = a :| (first : rest)

map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f (first :| rest) = f first :| P.map f rest

filter :: (a -> Bool) -> NonEmpty a -> [a]
filter keep list = P.filter keep (toList list)

zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f (a :| as) (b :| bs) = f a b :| P.zipWith f as bs

reverse :: NonEmpty a -> NonEmpty a
reverse list = fromList (P.reverse (toList list))

-- Sorted by what the function gives. Elements that give the same keep
-- their order.
sortWith :: Ord b => (a -> b) -> NonEmpty a -> NonEmpty a
sortWith key list = fromList (List.sortBy (\a b -> compare (key a) (key b)) (toList list))
