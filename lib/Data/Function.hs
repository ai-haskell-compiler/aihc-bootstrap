module Data.Function (module Data.Function, module Prelude) where

infixl 0 `on`

infixl 1 &

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on combine f a b = combine (f a) (f b)

(&) :: a -> (a -> b) -> b
(&) a f = f a

fix :: (a -> a) -> a
fix f = let result = f result in result
