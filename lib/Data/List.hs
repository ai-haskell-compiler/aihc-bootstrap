module Data.List (module Prelude) where

-- The list functions that GHC's Prelude does not export. Where the order of
-- a result is not obvious (nubBy, sortBy, maximumBy, ...) the definition
-- follows GHC's, because that order can end up in compiled output.

infix 5 \\

foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f (first : rest) = foldl f first rest
foldl1 _ [] = error "foldl1: empty list"

foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ [a] = a
foldr1 f (first : rest) = f first (foldr1 f rest)
foldr1 _ [] = error "foldr1: empty list"

singleton :: a -> [a]
singleton a = [a]

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (first : rest) = Just (first, rest)

unsnoc :: [a] -> Maybe ([a], a)
unsnoc [] = Nothing
unsnoc list = Just (init list, last list)

nub :: Eq a => [a] -> [a]
nub = nubBy (==)

-- The first of each group of equal elements. The test is applied to an
-- element already kept and the new one, in that order.
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy same list = go list []
  where
    go [] _ = []
    go (candidate : rest) kept
      | seen candidate kept = go rest kept
      | otherwise = candidate : go rest (candidate : kept)
    seen _ [] = False
    seen candidate (old : others) = same old candidate || seen candidate others

delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ _ [] = []
deleteBy same a (b : rest) = if same a b then rest else b : deleteBy same a rest

(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl (flip delete)

union :: Eq a => [a] -> [a] -> [a]
union list other = list ++ foldl (flip delete) (nub other) list

intersect :: Eq a => [a] -> [a] -> [a]
intersect list other = [a | a <- list, any (== a) other]

insert :: Ord a => a -> [a] -> [a]
insert = insertBy compare

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _ a [] = [a]
insertBy order a list@(b : rest) = case order a b of
  GT -> b : insertBy order a rest
  _ -> a : list

sort :: Ord a => [a] -> [a]
sort = sortBy compare

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn key list = map snd (sortBy (\a b -> compare (fst a) (fst b)) (map (\a -> let k = key a in seq k (k, a)) list))

-- A merge sort. Equal elements keep their order, as in GHC.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy order list = mergeAll (map (\a -> [a]) list)
  where
    mergeAll [] = []
    mergeAll [run] = run
    mergeAll runs = mergeAll (mergePairs runs)
    mergePairs (a : b : rest) = merge a b : mergePairs rest
    mergePairs runs = runs
    merge [] other = other
    merge run [] = run
    merge run@(a : as) other@(b : bs) = case order a b of
      GT -> b : merge run bs
      _ -> a : merge as other

-- The last of the largest elements.
maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy order = foldl1 (\a b -> case order a b of
  GT -> a
  _ -> b)

-- The first of the smallest elements.
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy order = foldl1 (\a b -> case order a b of
  GT -> b
  _ -> a)

find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find wanted (a : rest) = if wanted a then Just a else find wanted rest

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex a = findIndex (== a)

findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex wanted list = go 0 list
  where
    go _ [] = Nothing
    go n (a : rest) = if wanted a then Just n else go (n + 1) rest

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a = findIndices (== a)

findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices wanted list = [n | (n, a) <- zip [0 ..] list, wanted a]

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition wanted = foldr select ([], [])
  where
    select a rest =
      let (yes, no) = rest
       in if wanted a then (a : yes, no) else (yes, a : no)

mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumL _ state [] = (state, [])
mapAccumL f state (a : rest) = (final, b : others)
  where
    (next, b) = f state a
    (final, others) = mapAccumL f next rest

mapAccumR :: (s -> a -> (s, b)) -> s -> [a] -> (s, [b])
mapAccumR _ state [] = (state, [])
mapAccumR f state (a : rest) = (final, b : others)
  where
    (final, b) = f next a
    (next, others) = mapAccumR f state rest

intercalate :: [a] -> [[a]] -> [a]
intercalate separator lists = concat (intersperse separator lists)

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [a] = [a]
intersperse separator (a : rest) = a : separator : intersperse separator rest

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : rest) = transpose rest
transpose ((a : as) : rest) =
  (a : [h | h : _ <- rest]) : transpose (as : [t | _ : t <- rest])

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (a : as) (b : bs) = a == b && isPrefixOf as bs

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf suffix list = isPrefixOf (reverse suffix) (reverse list)

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] list = Just list
stripPrefix (a : as) (b : bs) = if a == b then stripPrefix as bs else Nothing
stripPrefix _ [] = Nothing

tails :: [a] -> [[a]]
tails [] = [[]]
tails list@(_ : rest) = list : tails rest

inits :: [a] -> [[a]]
inits [] = [[]]
inits (a : rest) = [] : map (a :) (inits rest)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd skip = foldr (\a rest -> if skip a && null rest then [] else a : rest) []

group :: Eq a => [a] -> [[a]]
group = groupBy (==)

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _ [] = []
groupBy same (a : rest) = case span (same a) rest of
  (alike, others) -> (a : alike) : groupBy same others

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr step seed = case step seed of
  Nothing -> []
  Just (a, next) -> a : unfoldr step next

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ start [] = [start]
scanl f start (a : rest) = start : scanl f (f start a) rest

genericLength :: Num i => [a] -> i
genericLength [] = 0
genericLength (_ : rest) = 1 + genericLength rest

zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 (a : as) (b : bs) (c : cs) (d : ds) = (a, b, c, d) : zip4 as bs cs ds
zip4 _ _ _ _ = []
