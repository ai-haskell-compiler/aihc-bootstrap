module GHC.Generics where

-- AIHC derives Generic so that other classes can be derived from it. The
-- interpreter derives those classes itself (src/derive.c), so the class
-- has nothing in it.

class Generic a

newtype Generically a = Generically a
