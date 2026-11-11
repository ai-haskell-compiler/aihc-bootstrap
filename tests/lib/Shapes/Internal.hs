{-# LANGUAGE PatternSynonyms #-}

module Shapes.Internal
  ( Shape (Circle, Square, Unit),
    Size (Size, width),
    area,
    pattern Tiny,
    Label,
    label,
  )
where

data Shape = Circle Int | Square Int | Hidden

data Size = SizeInternal Int Int

newtype Label = Label String

-- Exported as part of Shape: importing Shape (..) brings it.
pattern Unit :: Shape
pattern Unit = Square 1

-- Exported on its own: it has to be imported by name.
pattern Tiny :: Shape
pattern Tiny = Circle 0

-- Exported as part of Size, with one of its two fields.
pattern Size :: Int -> Int -> Size
pattern Size {width, height} <- SizeInternal width height
  where
    Size w h = SizeInternal w h

area :: Shape -> Int
area shape = case shape of
  Circle r -> 3 * r * r
  Square s -> s * s
  Hidden -> helper

label :: String -> Label
label = Label

-- Not exported. The importing module has its own definitions of these
-- names.
helper :: Int
helper = -1

describe :: Shape -> String
describe _ = "internal"
