module Data.Either (module Prelude) where

lefts :: [Either a b] -> [a]
lefts list = [a | Left a <- list]

rights :: [Either a b] -> [b]
rights list = [b | Right b <- list]

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers list = (lefts list, rights list)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True

fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft fallback (Right _) = fallback

fromRight :: b -> Either a b -> b
fromRight fallback (Left _) = fallback
fromRight _ (Right b) = b
