module Data.Void where

-- A type without values.
data Void

absurd :: Void -> a
absurd _ = error "Data.Void.absurd"

instance Eq Void where
  (==) _ _ = True

instance Ord Void where
  compare _ _ = EQ

instance Show Void where
  show = absurd
