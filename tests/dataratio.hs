module Main where

import Data.Data (Data, gmapM, gmapQ, gmapT)
import Data.Ratio (denominator, numerator, (%))
import Data.Typeable (cast)

-- A fraction has two fields for Data: its numerator and its denominator.
double :: (Data d) => d -> d
double d = case cast d of
  Just n -> case cast (n * 2 :: Integer) of
    Just doubled -> doubled
    Nothing -> d
  Nothing -> d

main :: IO ()
main = do
  let r = 3 % 4 :: Rational
  print (gmapQ (\d -> (cast d :: Maybe Integer)) r)
  print (gmapT double (3 % 5 :: Rational))
  result <- gmapM (\d -> do print (cast d :: Maybe Integer); pure d) r
  print (numerator result, denominator result)
