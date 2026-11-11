module Control.DeepSeq where

infixr 0 $!!, `deepseq`

-- Evaluating a value completely is the same for every type here, so the
-- class needs no instances beyond empty ones.
class NFData a where
  rnf :: a -> ()
  rnf = deepForce

deepseq :: a -> b -> b
deepseq a b = seq (deepForce a) b

force :: a -> a
force a = deepseq a a

($!!) :: (a -> b) -> a -> b
($!!) f a = deepseq a (f a)

foreign import prim "deepForce" deepForce :: a -> ()
