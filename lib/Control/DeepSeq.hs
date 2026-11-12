module Control.DeepSeq where

infixr 0 $!!, `deepseq`

-- Evaluating a value completely is the same for every type here, so the
-- class needs no instances beyond empty ones.
class NFData a where
  rnf :: a -> ()
  rnf = deepForce

-- Every type is an instance. An instance that a program gives for a type
-- of its own comes first.
instance NFData a

deepseq :: a -> b -> b
deepseq a b = seq (deepForce a) b

force :: a -> a
force a = deepseq a a

($!!) :: (a -> b) -> a -> b
($!!) f a = deepseq a (f a)

foreign import prim "deepForce" deepForce :: a -> ()
