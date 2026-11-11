module Data.Data (module Data.Data, Typeable, cast) where

-- Looking at the fields of a value without knowing its type: the three
-- functions that AIHC's generic traversals are made of.
--
-- Upstream defines them through gfoldl. Here a derived instance gets them
-- from the interpreter, which knows the fields of every value and their
-- types (dataFields, in src/derive.c). The other methods are there for
-- instances written by hand.

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Base (Map)
import qualified Data.Map.Base as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import Data.Word

class Data a where
  gfoldl :: (forall d b. Data d => c (d -> b) -> d -> c b) -> (forall g. g -> c g) -> a -> c a
  gfoldl _ z = z

  gunfold :: (forall b r. Data b => c (b -> r) -> c r) -> (forall r. r -> c r) -> Constr -> c a

  toConstr :: a -> Constr

  dataTypeOf :: a -> DataType

  -- The value with the function applied to each of its fields.
  gmapT :: (forall b. Data b => b -> b) -> a -> a
  gmapT _ a = a

  -- What the function gives for each field.
  gmapQ :: (forall d. Data d => d -> u) -> a -> [u]
  gmapQ _ _ = []

  -- As gmapT, with an action for each field, from left to right.
  gmapM :: Monad m => (forall d. Data d => d -> m d) -> a -> m a
  gmapM _ a = return a

-- ---- For derived instances ----

-- A field of a value, with its type.
data Field = forall d. Data d => Field d

derivedGmapT :: forall a. (forall b. Data b => b -> b) -> a -> a
derivedGmapT f a = dataRebuild a [Field (f d) | Field d <- dataFields a]

derivedGmapQ :: forall a u. (forall d. Data d => d -> u) -> a -> [u]
derivedGmapQ f a = [f d | Field d <- dataFields a]

derivedGmapM :: forall a m. Monad m => (forall d. Data d => d -> m d) -> a -> m a
derivedGmapM f a = fmap (dataRebuild a) (visit (dataFields a))
  where
    visit fields = case fields of
      [] -> return []
      Field d : rest -> f d >>= \d' -> visit rest >>= \rest' -> return (Field d' : rest')

-- The fields of the value, in order.
foreign import prim "dataFields" dataFields :: a -> [Field]

-- The value with its fields replaced by those of the list.
foreign import prim "dataRebuild" dataRebuild :: a -> [Field] -> a

-- ---- Descriptions of types, for instances written by hand ----

data Fixity = Prefix | Infix
  deriving (Eq, Show)

data DataType = DataType String [Constr]

data Constr = Constr String [String] Fixity

instance Eq Constr where
  (==) (Constr a _ _) (Constr b _ _) = a == b

instance Show Constr where
  show (Constr name _ _) = name

mkDataType :: String -> [Constr] -> DataType
mkDataType = DataType

mkConstr :: DataType -> String -> [String] -> Fixity -> Constr
mkConstr _ name fields fixity = Constr name fields fixity

dataTypeName :: DataType -> String
dataTypeName (DataType name _) = name

dataTypeConstrs :: DataType -> [Constr]
dataTypeConstrs (DataType _ constructors) = constructors

showConstr :: Constr -> String
showConstr (Constr name _ _) = name

constrFields :: Constr -> [String]
constrFields (Constr _ fields _) = fields

constrFixity :: Constr -> Fixity
constrFixity (Constr _ _ fixity) = fixity

-- ---- Types without fields ----

instance Data Int

instance Data Int8

instance Data Int16

instance Data Int32

instance Data Int64

instance Data Integer

instance Data Word

instance Data Word8

instance Data Word16

instance Data Word32

instance Data Word64

instance Data Char

instance Data Bool

instance Data Ordering

instance Data ()

-- ---- Types with fields ----

instance Data a => Data [a] where
  gmapT f list = case list of
    [] -> []
    x : xs -> f x : f xs
  gmapQ f list = case list of
    [] -> []
    x : xs -> [f x, f xs]
  gmapM f list = case list of
    [] -> return []
    x : xs -> f x >>= \x' -> f xs >>= \xs' -> return (x' : xs')

instance (Data a, Data b) => Data (a, b) where
  gmapT f (a, b) = (f a, f b)
  gmapQ f (a, b) = [f a, f b]
  gmapM f (a, b) = f a >>= \a' -> f b >>= \b' -> return (a', b')

instance (Data a, Data b, Data c) => Data (a, b, c) where
  gmapT f (a, b, c) = (f a, f b, f c)
  gmapQ f (a, b, c) = [f a, f b, f c]
  gmapM f (a, b, c) = f a >>= \a' -> f b >>= \b' -> f c >>= \c' -> return (a', b', c')

instance Data a => Data (Maybe a) where
  gmapT = derivedGmapT
  gmapQ = derivedGmapQ
  gmapM = derivedGmapM

instance (Data a, Data b) => Data (Either a b) where
  gmapT = derivedGmapT
  gmapQ = derivedGmapQ
  gmapM = derivedGmapM

instance Data a => Data (NonEmpty a) where
  gmapT = derivedGmapT
  gmapQ = derivedGmapQ
  gmapM = derivedGmapM

-- The types below show one field, as upstream does: the list they are
-- made from.

instance Data Text where
  gmapT f text = T.pack (f (T.unpack text))
  gmapQ f text = [f (T.unpack text)]
  gmapM f text = fmap T.pack (f (T.unpack text))

instance Data ByteString where
  gmapT f bytes = B.pack (f (B.unpack bytes))
  gmapQ f bytes = [f (B.unpack bytes)]
  gmapM f bytes = fmap B.pack (f (B.unpack bytes))

instance (Data k, Data a, Ord k) => Data (Map k a) where
  gmapT f m = Map.fromList (f (Map.toList m))
  gmapQ f m = [f (Map.toList m)]
  gmapM f m = fmap Map.fromList (f (Map.toList m))

instance (Data a, Ord a) => Data (Set a) where
  gmapT f set = Set.fromList (f (Set.toList set))
  gmapQ f set = [f (Set.toList set)]
  gmapM f set = fmap Set.fromList (f (Set.toList set))
