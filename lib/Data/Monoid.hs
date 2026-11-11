module Data.Monoid where

-- Types whose only purpose is the way their values combine.

newtype Sum a = Sum {getSum :: a}
  deriving (Eq, Ord, Show)

instance Num a => Semigroup (Sum a) where
  (<>) (Sum a) (Sum b) = Sum (a + b)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0

instance Num a => Num (Sum a) where
  (+) (Sum a) (Sum b) = Sum (a + b)
  (-) (Sum a) (Sum b) = Sum (a - b)
  (*) (Sum a) (Sum b) = Sum (a * b)
  negate (Sum a) = Sum (negate a)
  abs (Sum a) = Sum (abs a)
  signum (Sum a) = Sum (signum a)
  fromInteger n = Sum (fromInteger n)

newtype Product a = Product {getProduct :: a}
  deriving (Eq, Ord, Show)

instance Num a => Semigroup (Product a) where
  (<>) (Product a) (Product b) = Product (a * b)

instance Num a => Monoid (Product a) where
  mempty = Product 1

newtype Any = Any {getAny :: Bool}
  deriving (Eq, Ord, Show)

instance Semigroup Any where
  (<>) (Any a) (Any b) = Any (a || b)

instance Monoid Any where
  mempty = Any False

newtype All = All {getAll :: Bool}
  deriving (Eq, Ord, Show)

instance Semigroup All where
  (<>) (All a) (All b) = All (a && b)

instance Monoid All where
  mempty = All True

-- The first value that is there, and the last one.
newtype First a = First {getFirst :: Maybe a}
  deriving (Eq, Ord, Show)

instance Semigroup (First a) where
  (<>) (First Nothing) other = other
  (<>) first _ = first

instance Monoid (First a) where
  mempty = First Nothing

newtype Last a = Last {getLast :: Maybe a}
  deriving (Eq, Ord, Show)

instance Semigroup (Last a) where
  (<>) first (Last Nothing) = first
  (<>) _ other = other

instance Monoid (Last a) where
  mempty = Last Nothing

-- Functions from a type to itself, composed.
newtype Endo a = Endo {appEndo :: a -> a}

instance Semigroup (Endo a) where
  (<>) (Endo f) (Endo g) = Endo (f . g)

instance Monoid (Endo a) where
  mempty = Endo id
