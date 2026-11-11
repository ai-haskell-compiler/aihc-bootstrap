module Data.Functor.Identity where

newtype Identity a = Identity {runIdentity :: a}
  deriving (Eq, Ord)

instance Show a => Show (Identity a) where
  showsPrec precedence (Identity a) =
    showParen (precedence >= 11) (\rest -> "Identity " ++ showsPrec 11 a rest)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  (>>=) (Identity a) f = f a

instance Foldable Identity where
  foldr f end (Identity a) = f a end

instance Traversable Identity where
  traverse f (Identity a) = fmap Identity (f a)
