module Main where

data Colour = Red | Green | Blue

isRed :: Colour -> Bool
isRed Red = True
isRed _ = False

name :: Colour -> String
name Red = "red"
name Green = "green"
name Blue = "blue"

-- Guards fall through to the next clause when none holds.
classify :: Colour -> Bool -> String
classify colour flag
  | isRed colour, flag = "red and flagged"
  | isRed colour = "red"
classify Green _ = "green"
classify colour flag
  | flag = other ++ " and flagged"
  | otherwise = other
  where
    other = "other: " ++ name colour

-- Pattern guards bind variables for the rest of the guard and the body.
lookupName :: Maybe Colour -> String
lookupName found
  | Just colour <- found, isRed colour = "found red"
  | Just colour <- found = "found " ++ name colour
  | otherwise = "nothing"

-- Guards in case alternatives, falling through to the next alternative.
pick :: Maybe Colour -> String
pick value = case value of
  Just colour
    | isRed colour -> "case red"
  Just Green -> "case green"
  _ -> "case other"

-- A definition without parameters.
verdict :: String
verdict
  | isRed Blue = "wrong"
  | otherwise = "right"

main :: IO ()
main = do
  putStrLn (classify Red True)
  putStrLn (classify Red False)
  putStrLn (classify Green True)
  putStrLn (classify Blue True)
  putStrLn (classify Blue False)
  putStrLn (lookupName (Just Red))
  putStrLn (lookupName (Just Green))
  putStrLn (lookupName Nothing)
  putStrLn (pick (Just Red))
  putStrLn (pick (Just Green))
  putStrLn (pick (Just Blue))
  putStrLn verdict
