module Main where

import Prelude hiding (Maybe (..), Either (..), id)

data Colour = Red | Green

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

newtype Identity a = Identity a

-- A method that takes its argument at the class type.
class Describe a where
  describe :: a -> String
  describeTwice :: a -> IO ()
  describeTwice value = putStrLn (describe value) >> putStrLn (describe value)

instance Describe Colour where
  describe colour = case colour of
    Red -> "red"
    Green -> "green"

-- An instance that needs another instance at a type known only at run time.
instance Describe a => Describe (Maybe a) where
  describe value = case value of
    Nothing -> "nothing"
    Just inner -> describe inner
  describeTwice value = putStrLn "maybe, once"

instance (Describe a, Describe b) => Describe (Either a b) where
  describe value = case value of
    Left a -> describe a
    Right b -> describe b

-- A method whose type is decided by how its result is used.
class Default a where
  def :: a

instance Default Colour where
  def = Green

instance Default a => Default (Maybe a) where
  def = Just def

instance Default b => Default (Either a b) where
  def = Right def

-- A class over type constructors, with a second monad to tell apart from IO.
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  (>>=) (Identity a) next = next a

runIdentity :: Identity a -> a
runIdentity (Identity a) = a

id :: a -> a
id a = a

-- The monad is chosen by the caller.
pick :: Monad m => m Colour
pick = return Red >>= (\_ -> return Green)

both :: (Describe a, Describe b) => a -> b -> IO ()
both first second = do
  putStrLn (describe first)
  putStrLn (describe second)

-- No signature: the type is inferred and generalised.
describeDefault unused = describe (def :: Either Colour (Maybe Colour))

main :: IO ()
main = do
  putStrLn (describe Red)
  putStrLn (describe (Just (Just Green)))
  putStrLn (describe (Nothing :: Maybe Colour))
  putStrLn (describe (def :: Maybe Colour))
  putStrLn (describeDefault Red)
  both (Left Red :: Either Colour Colour) (Just Green)
  describeTwice Red
  describeTwice (Just Red)
  colour <- pick
  putStrLn (describe colour)
  putStrLn (describe (runIdentity pick))
  putStrLn (describe (runIdentity (fmap Just (pure id <*> Identity Red))))
