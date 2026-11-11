module Data.Set where

-- A set is a map from its elements to nothing.

import Control.DeepSeq (NFData)
import qualified Data.Map.Base as M
import Prelude hiding (filter, foldr, map, null)
import qualified Prelude as P

infixl 9 \\

newtype Set a = Set (M.Map a ())

empty :: Set a
empty = Set M.empty

singleton :: a -> Set a
singleton a = Set (M.singleton a ())

insert :: Ord a => a -> Set a -> Set a
insert a (Set m) = Set (M.insert a () m)

delete :: Ord a => a -> Set a -> Set a
delete a (Set m) = Set (M.delete a m)

fromList :: Ord a => [a] -> Set a
fromList = P.foldl' (flip insert) empty

fromAscList :: Ord a => [a] -> Set a
fromAscList = fromList

fromDistinctAscList :: [a] -> Set a
fromDistinctAscList list = Set (M.fromDistinctAscList (P.map (\a -> (a, ())) list))

null :: Set a -> Bool
null (Set m) = M.null m

size :: Set a -> Int
size (Set m) = M.size m

member :: Ord a => a -> Set a -> Bool
member a (Set m) = M.member a m

notMember :: Ord a => a -> Set a -> Bool
notMember a set = not (member a set)

toAscList :: Set a -> [a]
toAscList (Set m) = M.keys m

toDescList :: Set a -> [a]
toDescList (Set m) = P.map fst (M.toDescList m)

toList :: Set a -> [a]
toList = toAscList

elems :: Set a -> [a]
elems = toAscList

foldr :: (a -> b -> b) -> b -> Set a -> b
foldr f end (Set m) = M.foldrWithKey (\a _ rest -> f a rest) end m

foldl' :: (b -> a -> b) -> b -> Set a -> b
foldl' f start set = P.foldl' f start (toAscList set)

foldl :: (b -> a -> b) -> b -> Set a -> b
foldl f start set = P.foldl f start (toAscList set)

union :: Ord a => Set a -> Set a -> Set a
union (Set left) (Set right) = Set (M.union left right)

unions :: Ord a => [Set a] -> Set a
unions = P.foldl' union empty

difference :: Ord a => Set a -> Set a -> Set a
difference (Set left) (Set right) = Set (M.difference left right)

(\\) :: Ord a => Set a -> Set a -> Set a
(\\) = difference

intersection :: Ord a => Set a -> Set a -> Set a
intersection (Set left) (Set right) = Set (M.intersection left right)

disjoint :: Ord a => Set a -> Set a -> Bool
disjoint left right = null (intersection left right)

isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf left right = all (\a -> member a right) (toAscList left)

isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf left right = size left < size right && isSubsetOf left right

filter :: (a -> Bool) -> Set a -> Set a
filter keep (Set m) = Set (M.filterWithKey (\a _ -> keep a) m)

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition keep set = (filter keep set, filter (not . keep) set)

map :: Ord b => (a -> b) -> Set a -> Set b
map f set = fromList (P.map f (toAscList set))

lookupMin :: Set a -> Maybe a
lookupMin (Set m) = fmap fst (M.lookupMin m)

lookupMax :: Set a -> Maybe a
lookupMax (Set m) = fmap fst (M.lookupMax m)

findMin :: Set a -> a
findMin set = case lookupMin set of
  Just a -> a
  Nothing -> error "Set.findMin: empty set has no minimal element"

findMax :: Set a -> a
findMax set = case lookupMax set of
  Just a -> a
  Nothing -> error "Set.findMax: empty set has no maximal element"

deleteMin :: Set a -> Set a
deleteMin (Set m) = Set (M.deleteMin m)

deleteMax :: Set a -> Set a
deleteMax (Set m) = Set (M.deleteMax m)

minView :: Set a -> Maybe (a, Set a)
minView (Set m) = fmap (\((a, _), rest) -> (a, Set rest)) (M.minViewWithKey m)

maxView :: Set a -> Maybe (a, Set a)
maxView (Set m) = fmap (\((a, _), rest) -> (a, Set rest)) (M.maxViewWithKey m)

lookupLT :: Ord a => a -> Set a -> Maybe a
lookupLT a (Set m) = fmap fst (M.lookupLT a m)

lookupLE :: Ord a => a -> Set a -> Maybe a
lookupLE a (Set m) = fmap fst (M.lookupLE a m)

lookupGT :: Ord a => a -> Set a -> Maybe a
lookupGT a (Set m) = fmap fst (M.lookupGT a m)

lookupGE :: Ord a => a -> Set a -> Maybe a
lookupGE a (Set m) = fmap fst (M.lookupGE a m)

instance Eq a => Eq (Set a) where
  (==) left right = size left == size right && toAscList left == toAscList right

instance Ord a => Ord (Set a) where
  compare left right = compare (toAscList left) (toAscList right)

instance Show a => Show (Set a) where
  showsPrec precedence set =
    showParen (precedence > 10) (\rest -> "fromList " ++ shows (toAscList set) rest)

instance Foldable Set where
  foldr = foldr

instance Ord a => Semigroup (Set a) where
  (<>) = union

instance Ord a => Monoid (Set a) where
  mempty = empty

instance NFData (Set a)
