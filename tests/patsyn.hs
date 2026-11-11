{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Char (isDigit, isSpace)
import qualified Data.Map as Map

data Type
  = Con String [Type]
  | Var Int
  | Fun Type Type
  deriving (Eq, Show)

-- Both ways: a pattern and an expression.
pattern IntT :: Type
pattern IntT = Con "Int" []

pattern ListT :: Type -> Type
pattern ListT element = Con "List" [element]

pattern Arrow2 :: Type -> Type -> Type -> Type
pattern Arrow2 a b c = Fun a (Fun b c)

-- Only a pattern, through a function.
pattern Arity :: Int -> Type
pattern Arity n <- (arity -> n)

pattern Nullary :: Type
pattern Nullary <- (arity -> 0)

pattern Named, Applied :: String -> Type
pattern Named name <- Con name _
pattern Applied name <- Con name (_ : _)

arity :: Type -> Int
arity ty = case ty of
  Fun _ result -> 1 + arity result
  _ -> 0

-- A pattern with a clause that says what it is as an expression.
pattern Pair :: Type -> Type -> Type
pattern Pair a b <- Con "Pair" [a, b]
  where
    Pair a b = Con "Pair" [b, a]

-- Fields.
data Entry = EntryInternal String Int Bool

pattern Entry :: String -> Int -> Entry
pattern Entry {entryName, entrySize} <- EntryInternal entryName entrySize _

-- An operator, and one that looks into a string.
pattern (:>) :: [a] -> a -> [a]
pattern xs :> x <- (unsnoc -> Just (xs, x))

unsnoc :: [a] -> Maybe ([a], a)
unsnoc list = case list of
  [] -> Nothing
  _ -> Just (init list, last list)

pattern Digits :: String -> String -> String
pattern Digits digits rest <- (span isDigit -> (digits@(_ : _), rest))

pattern Blank :: String
pattern Blank <- (all isSpace -> True)

pattern Lookup :: Int -> Map.Map String Int
pattern Lookup n <- (Map.lookup "key" -> Just n)

describe :: Type -> String
describe ty = case ty of
  IntT -> "int"
  ListT IntT -> "list of int"
  ListT (ListT inner) -> "list of lists of " ++ describe inner
  ListT other -> "list of " ++ describe other
  Pair a b -> "pair of " ++ describe a ++ " and " ++ describe b
  Arrow2 a IntT c -> "from " ++ describe a ++ " and an int to " ++ describe c
  Arity n | n > 0 -> "function of " ++ show n
  Applied name -> "applied " ++ name
  Named name -> "named " ++ name
  Nullary -> "something else"

classify :: String -> String
classify text = case text of
  Blank -> "blank"
  Digits ds Blank -> "number " ++ ds
  Digits ds (c : _) -> "number " ++ ds ++ " then " ++ [c]
  _ :> '!' -> "shout"
  body :> c -> "ends in " ++ [c] ++ " after " ++ show (length body)
  _ -> "unreachable"

size :: Entry -> Int
size Entry {entrySize} = entrySize

rename :: Entry -> String
rename (Entry name n) = name ++ show n

main :: IO ()
main = do
  mapM_ (putStrLn . describe)
    [ IntT,
      ListT IntT,
      ListT (ListT (Var 1)),
      ListT (Var 2),
      Pair IntT (Var 3),
      Arrow2 (Var 1) IntT (ListT IntT),
      Fun IntT IntT,
      Con "Maybe" [IntT],
      Con "Unit" [],
      Var 9
    ]
  print (IntT, ListT IntT == Con "List" [IntT], Arrow2 IntT IntT IntT)
  print (map (\f -> f IntT (Var 0)) [Pair, flip Pair])
  mapM_ (putStrLn . classify) ["", "  ", "42", "42 ", "7up", "hey!", "abc"]
  let entry = EntryInternal "file" 12 True
  print (entryName entry, entrySize entry, size entry, rename entry)
  print (map entrySize [entry, EntryInternal "x" 0 False])
  print [n | Lookup n <- [Map.fromList [("key", 1)], Map.empty, Map.fromList [("key", 3), ("k", 0)]]]
  print (case [1, 2, 3 :: Int] of (_ :> a) :> b -> (a, b); _ -> (0, 0))
