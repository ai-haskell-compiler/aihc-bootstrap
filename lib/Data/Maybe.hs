module Data.Maybe where

fromMaybe :: a -> Maybe a -> a
fromMaybe fallback Nothing = fallback
fromMaybe _ (Just a) = a

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just _) = False

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : rest) = catMaybes rest
catMaybes (Just a : rest) = a : catMaybes rest

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f list = catMaybes (map f list)

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (a : _) = Just a

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

fromJust :: Maybe a -> a
fromJust found = case found of
  Just a -> a
  Nothing -> error "Maybe.fromJust: Nothing"
