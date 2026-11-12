module Interpreter.Derive where

-- Derived Functor, Foldable and Traversable.
--
-- All three go through the values of the last type parameter in the fields
-- of a value. The interpreter knows where those are (functorFields, in
-- src/derive.c); the walk itself is written once, here, as traverse, and
-- the other two are traverse with a suitable Applicative.

-- A field, or a part of one, of a type that is not looked at here.
data Any

-- Where the values of the last type parameter are in a field:
data Shape
  = Skip -- nowhere: the type of the field does not mention it
  | Element -- the field is one
  | -- Inside the values of another type: `walk` is the traverse of that
    -- type, and the shape is that of what it holds. A field [(op, a)] is
    -- Inside the list, Inside the pair, Element.
    Inside (forall f a b. Applicative f => (a -> f b) -> Any -> f Any) Shape

derivedTraverse :: forall t f a b. Applicative f => (a -> f b) -> t a -> f (t b)
derivedTraverse f value = fmap (functorRebuild value) (visitAll (functorFields value))
  where
    visitAll :: [(Shape, Any)] -> f [Any]
    visitAll fields = case fields of
      [] -> pure []
      (shape, field) : rest -> liftA2 (:) (visit shape field) (visitAll rest)
    visit :: Shape -> Any -> f Any
    visit shape field = case shape of
      Skip -> pure field
      Element -> fmap unsafeCoerce (f (unsafeCoerce field))
      Inside walk inner -> walk (visit inner) field

-- The Applicative that does nothing but apply the function.
newtype Plain a = Plain a

instance Functor Plain where
  fmap f (Plain a) = Plain (f a)

instance Applicative Plain where
  pure = Plain
  (<*>) (Plain f) (Plain a) = Plain (f a)

derivedFmap :: forall t a b. (a -> b) -> t a -> t b
derivedFmap f value = case derivedTraverse (\a -> Plain (f a)) value of
  Plain result -> result

-- The Applicative that only notes the elements it is given, in order: a
-- function that puts them in front of a list.
newtype Collect e a = Collect ([e] -> [e])

instance Functor (Collect e) where
  fmap _ (Collect elements) = Collect elements

instance Applicative (Collect e) where
  pure _ = Collect id
  (<*>) (Collect first) (Collect second) = Collect (first . second)

derivedFoldr :: forall t a b. (a -> b -> b) -> b -> t a -> b
derivedFoldr f end value = case derivedTraverse (\a -> Collect (a :)) value of
  Collect elements -> foldr f end (elements [])

-- The fields of the value, in order, each with its shape.
foreign import prim "functorFields" functorFields :: t a -> [(Shape, Any)]

-- The value with its fields replaced by those of the list.
foreign import prim "functorRebuild" functorRebuild :: t a -> [Any] -> t b

foreign import prim "unsafeCoerce" unsafeCoerce :: a -> b
