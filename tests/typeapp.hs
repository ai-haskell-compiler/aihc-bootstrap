{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.Int (Int8)
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable, cast)
import Data.Word (Word16, Word8)

-- The type variables of a signature are in the order they are written,
-- the context included.
convert :: forall a b. (Integral a, Num b) => a -> b
convert = fromIntegral

limits :: forall a. (Bounded a, Show a) => String
limits = show (minBound @a) ++ ".." ++ show (maxBound @a)

-- A type variable of the signature, used in the body.
sizes :: forall a. (Bounded a, Enum a) => [a] -> (Int, Int)
sizes list = (length list, fromEnum (maxBound @a) - fromEnum (minBound @a) + 1)

-- Values with their types, as AIHC's annotations are.
data Annotation = Span Int Int | Other Dynamic

annotation :: Typeable a => a -> Annotation
annotation value = case cast value of
  Just (from, to) -> Span from to
  Nothing -> Other (toDyn value)

fromAnnotation :: Typeable a => Annotation -> Maybe a
fromAnnotation (Span from to) = cast (from, to)
fromAnnotation (Other value) = fromDynamic value

newtype Name = Name String
  deriving (Show)

main :: IO ()
main = do
  print (convert @Int @Word8 300, convert @_ @Int8 (200 :: Int), convert @Integer 7 + (1 :: Word16))
  print (fromIntegral @Int @Word8 511, toEnum @Char 65, fromEnum @Bool True)
  print (maxBound @Int8, minBound @Word8, [minBound @Bool ..])
  print (show @Integer 5, show @[Int] [], compare @Int 1 2)
  putStrLn (limits @Word8)
  putStrLn (limits @Bool)
  print (sizes "abc" == (3, 1114112), sizes [True], sizes @Int8 [])
  print (either @String @Int length (+ 1) (Left "abc"), maybe @Int 0 (* 2) (Just 4))
  print (map (fromIntegral @Word8 @Int) [1, 255], pure @Maybe 'x', mempty @String)
  let annotations = [annotation (1 :: Int, 2 :: Int), annotation (Name "n"), annotation "text", annotation (Name "m"), annotation True]
  print (mapMaybe (fromAnnotation @Name) annotations)
  print (mapMaybe (fromAnnotation @(Int, Int)) annotations)
  print (mapMaybe (fromAnnotation @String) annotations, mapMaybe (fromAnnotation @Bool) annotations)
  print (length (mapMaybe (fromAnnotation @Int) annotations))
  print (fmap (+ 1) (fromDynamic @Int (toDyn (41 :: Int))), fromDynamic @Int (toDyn 'c'))
