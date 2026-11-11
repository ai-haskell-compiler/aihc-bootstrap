{-# LANGUAGE MagicHash #-}

module GHC.Exts where

-- Two names AIHC uses to ask whether two values are the same object in
-- memory, as a shortcut before comparing them. Saying no is always a
-- correct answer: the comparison is then done in full.

reallyUnsafePtrEquality# :: a -> a -> Bool
reallyUnsafePtrEquality# _ _ = False

isTrue# :: Bool -> Bool
isTrue# b = b
