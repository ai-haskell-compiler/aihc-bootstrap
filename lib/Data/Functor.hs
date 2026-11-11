module Data.Functor (module Prelude) where

infixl 4 $>

infixl 1 <&>

($>) :: Functor f => f a -> b -> f b
($>) action b = fmap (\_ -> b) action

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) action f = fmap f action

void :: Functor f => f a -> f ()
void = fmap (\_ -> ())
