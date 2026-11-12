{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.State.Strict (State, evalState, get, put, runState)
import Data.Data (Constr, Data (..), DataType, Fixity (Prefix), Typeable, cast, mkConstr, mkDataType)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Typeable (eqT, (:~:) (Refl))
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype Name = Name String
  deriving (Eq, Ord, Show, Data, Generic)

data Expr
  = Var Name
  | Lit Int
  | App Expr [Expr]
  | Lam [Name] Expr
  | Let (Map.Map Name Expr) Expr
  | Noted Note Expr
  | Tuple (Expr, Maybe Expr)
  deriving (Eq, Show, Data, Generic)

data Decl = Decl {declName :: Name, declBody :: Expr, declTags :: Set.Set Text}
  deriving (Eq, Show, Data, Generic)

-- A type whose instance is written by hand and has no fields to show.
data Note = Note String Int
  deriving (Eq, Show)

instance Data Note where
  gfoldl _ z = z
  gunfold _ z _ = z (Note "" 0)
  toConstr _ = noteConstr
  dataTypeOf _ = noteDataType

noteConstr :: Constr
noteConstr = mkConstr noteDataType "Note" [] Prefix

noteDataType :: DataType
noteDataType = mkDataType "Main.Note" [noteConstr]

-- ---- The usual traversals, written with the three methods ----

everything :: (r -> r -> r) -> (forall a. Data a => a -> r) -> (forall a. Data a => a -> r)
everything combine query value = foldl combine (query value) (gmapQ (everything combine query) value)

everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f value = f (gmapT (everywhere f) value)

everywhereM :: Monad m => (forall a. Data a => a -> m a) -> (forall a. Data a => a -> m a)
everywhereM f value = gmapM (everywhereM f) value >>= f

mkQ :: (Typeable a, Typeable b) => r -> (b -> r) -> a -> r
mkQ fallback query value = maybe fallback query (cast value)

mkT :: (Typeable a, Typeable b) => (b -> b) -> a -> a
mkT f value = fromMaybe value (cast . f =<< cast value)

extT :: (Typeable a, Typeable b) => (a -> a) -> (b -> b) -> a -> a
extT fallback f value = fromMaybe (fallback value) (cast . f =<< cast value)

mkM :: forall m a b. (Monad m, Typeable a, Typeable b) => (b -> m b) -> a -> m a
mkM f value = case cast value of
  Just b -> f b >>= \b' -> pure (fromMaybe value (cast b'))
  Nothing -> pure value

-- ---- Uses ----

names :: Data a => a -> [Name]
names = everything (++) (mkQ [] (\name -> [name :: Name]))

literals :: Data a => a -> [Int]
literals = everything (++) (mkQ [] literal)
  where
    literal expr = case expr of
      Lit n -> [n]
      _ -> []

characters :: Data a => a -> Int
characters = everything (+) (mkQ 0 (\c -> if (c :: Char) == 'x' then 1 else 0))

rename :: Data a => a -> a
rename = everywhere (mkT (\(Name n) -> Name (n ++ "'")))

simplify :: Data a => a -> a
simplify = everywhere (id `extT` unNote `extT` double)
  where
    unNote expr = case expr of
      Noted _ inner -> inner
      _ -> expr
    double n = n * 2 :: Int

-- Numbers every literal in the order the traversal reaches it.
number :: Data a => a -> (a, Int)
number value = runState (everywhereM (mkM step) value) 0
  where
    step :: Expr -> State Int Expr
    step expr = case expr of
      Lit _ -> do
        n <- get
        put (n + 1)
        pure (Lit n)
      _ -> pure expr

sizes :: Data a => a -> [Int]
sizes value = gmapQ (\field -> length (gmapQ (const ()) field)) value

example :: Expr
example =
  Let
    (Map.fromList [(Name "y", Lit 2), (Name "x", App (Var (Name "f")) [Lit 1, Noted (Note "n" 9) (Lit 5)])])
    (Lam [Name "a", Name "b"] (Tuple (Var (Name "x"), Just (App (Var (Name "a")) [Var (Name "y"), Lit 3]))))

decl :: Decl
decl = Decl (Name "main") example (Set.fromList [T.pack "export", T.pack "xx"])

-- eqT: whether two types are the same type. Where they are, a value of
-- the one is a value of the other.
data Shrub = Bud Int | Fork Shrub String Shrub deriving (Data)

everyInt :: forall value. (Data value) => value -> [Int]
everyInt value =
  case eqT @value @Int of
    Just Refl -> [value]
    Nothing -> concat (gmapQ everyInt value)

sameType :: forall a b. (Data a, Data b) => a -> b -> Bool
sameType _ _ = case eqT @a @b of
  Just Refl -> True
  Nothing -> False

main :: IO ()
main = do
  print (names example)
  print (names decl)
  print (literals example, literals [Lit 7, Var (Name "q")], literals (Just (Lit 8), 'c'))
  print (characters decl, characters (T.pack "xyx"), characters ("x", [Just 'x', Nothing]))
  print (rename (Lam [Name "a"] (Var (Name "a"))))
  print (rename decl == decl, declName (rename decl), Map.keys (case declBody (rename decl) of Let m _ -> m; _ -> Map.empty))
  print (simplify (App (Noted (Note "gone" 1) (Lit 1)) [Lit 2, Noted (Note "too" 2) (Var (Name "v"))]))
  print (number (App (Lit 0) [Lit 0, Tuple (Lit 0, Just (Lit 0))]))
  print (fst (number decl) == decl, snd (number decl))
  print (sizes example, sizes (Lit 1), sizes (Name "abc"), sizes (1 :: Int, "ab", Just True))
  print (gmapQ (mkQ "other" (\(Name n) -> n)) (Lam [Name "p"] (Var (Name "q"))))
  print (gmapT (mkT (+ (1 :: Int))) (3 :: Int, 4 :: Int, 'c'), gmapT (mkT not) [True, False])
  print (gmapM (mkM (\n -> if n > (0 :: Int) then Just n else Nothing)) (1 :: Int, 2 :: Int), gmapM (mkM (\n -> if n > (0 :: Int) then Just n else Nothing)) (1 :: Int, 0 :: Int))
  print (gmapQ (const 'f') (Note "x" 1), gmapT (mkT (+ (1 :: Int))) (Note "x" 1), length (gmapQ (const ()) (Map.fromList [(1 :: Int, 'a'), (2, 'b')])))
  print (evalState (everywhereM (mkM (\(Name n) -> get >>= \k -> put (k + 1) >> pure (Name (n ++ show (k :: Int))))) [Name "a", Name "b", Name "c"]) 0)
  print (everyInt (Fork (Bud 1) "x" (Fork (Bud 2) "y" (Bud 3))))
  print (everyInt (5 :: Int), everyInt "none", everyInt [(1 :: Int, 'a'), (2, 'b')])
  print (sameType 'a' 'b', sameType 'a' "b", sameType (1 :: Int) (2 :: Int))
