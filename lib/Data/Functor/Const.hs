module Data.Functor.Const where

-- A functor that ignores what it is mapped over: used to collect a value.

newtype Const a b = Const {getConst :: a}
  deriving (Eq, Ord, Show)

instance Functor (Const m) where
  fmap _ (Const a) = Const a

instance Foldable (Const m) where
  foldr _ end _ = end

instance Traversable (Const m) where
  traverse _ (Const a) = pure (Const a)

instance Monoid m => Applicative (Const m) where
  pure _ = Const mempty
  (<*>) (Const a) (Const b) = Const (a <> b)
