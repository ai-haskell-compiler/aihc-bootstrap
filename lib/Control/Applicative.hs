module Control.Applicative (module Control.Applicative, module Data.Functor.Const) where

import Data.Functor.Const

infixl 3 <|>

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a
  some :: f a -> f [a]
  many :: f a -> f [a]
  some action = liftA2 (:) action (many action)
  many action = some action <|> pure []

instance Alternative Maybe where
  empty = Nothing
  (<|>) Nothing other = other
  (<|>) found _ = found

instance Alternative [] where
  empty = []
  (<|>) = (++)

optional :: Alternative f => f a -> f (Maybe a)
optional action = fmap Just action <|> pure Nothing

asum :: (Foldable t, Alternative f) => t (f a) -> f a
asum = foldr (<|>) empty

liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = pure f <*> a

liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f a b c = liftA2 f a b <*> c

infixl 4 <**>

(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = liftA2 (\a f -> f a)
