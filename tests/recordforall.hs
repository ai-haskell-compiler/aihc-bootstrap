{-# LANGUAGE RankNTypes #-}
module Main where

data Box a = Box Int a | Plain a
  deriving (Show)

-- A record with a field that works at every type.
data Walk = Walk
  { walkInt :: Int -> Int,
    walkBox :: forall a. (Show a) => Box a -> String
  }

idWalk :: Walk
idWalk = Walk {walkInt = id, walkBox = const "-"}

-- A record update gives the field a new value. The value is used at the
-- field's own type variable, which it needs when it runs: show at Box a.
describing :: Walk
describing =
  idWalk
    { walkInt = (+ 1),
      walkBox = describe "box"
    }
  where
    describe :: (Show node) => String -> node -> String
    describe label node = label ++ ": " ++ show node

main :: IO ()
main = do
  print (walkInt describing 1)
  putStrLn (walkBox describing (Box 3 'x'))
  putStrLn (walkBox describing (Plain True))
  putStrLn (walkBox idWalk (Box 4 "s"))
