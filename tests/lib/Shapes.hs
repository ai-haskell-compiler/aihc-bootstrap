module Shapes
  ( module Shapes.Internal,
    describe,
    double,
  )
where

import Shapes.Internal

describe :: Shape -> String
describe shape = "shape of area " ++ show (area shape)

double :: Shape -> Shape
double shape = case shape of
  Circle r -> Circle (2 * r)
  Square s -> Square (2 * s)
  other -> other

-- Not exported.
secret :: Int
secret = 42
