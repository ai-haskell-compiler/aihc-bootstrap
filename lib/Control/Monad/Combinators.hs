-- This file is derived from Control.Monad.Combinators in the package
-- parser-combinators, Copyright © 2017–present Mark Karpov, and is under
-- that package's licence: see licenses/parser-combinators.txt.

module Control.Monad.Combinators
  ( module Control.Monad.Combinators,
    (<|>),
    empty,
  )
where

-- Ways to combine parsers, for any monad with a choice. The definitions
-- are upstream's: which parser runs when decides what a parse error says.

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (when)

many :: (Monad m, Alternative m) => m a -> m [a]
many p = go id
  where
    go f = optional p >>= \r -> case r of
      Nothing -> return (f [])
      Just x -> go (f . (x :))

some :: (Monad m, Alternative m) => m a -> m [a]
some p = p >>= \x -> fmap (x :) (many p)

optional :: Alternative m => m a -> m (Maybe a)
optional p = fmap Just p <|> pure Nothing

option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x

choice :: Alternative m => [m a] -> m a
choice = foldr (<|>) empty

between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close

count :: Monad m => Int -> m a -> m [a]
count n p =
  if n <= 0
    then return []
    else p >>= \x -> fmap (x :) (count (n - 1) p)

-- At least `low` and at most `high` times.
count' :: (Monad m, Alternative m) => Int -> Int -> m a -> m [a]
count' low high p
  | high <= 0 || low > high = return []
  | low > 0 = p >>= \x -> fmap (x :) (count' (low - 1) (high - 1) p)
  | otherwise = optional p >>= \r -> case r of
      Nothing -> return []
      Just x -> fmap (x :) (count' 0 (high - 1) p)

sepBy :: (Monad m, Alternative m) => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: (Monad m, Alternative m) => m a -> m sep -> m [a]
sepBy1 p sep = p >>= \x -> fmap (x :) (many (sep >> p))

sepEndBy :: (Monad m, Alternative m) => m a -> m sep -> m [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

sepEndBy1 :: (Monad m, Alternative m) => m a -> m sep -> m [a]
sepEndBy1 p sep = p >>= \x ->
  option False (fmap (const True) sep) >>= \more ->
    if more then fmap (x :) (sepEndBy p sep) else return [x]

endBy :: (Monad m, Alternative m) => m a -> m sep -> m [a]
endBy p sep = many (p >>= \x -> sep >> return x)

endBy1 :: (Monad m, Alternative m) => m a -> m sep -> m [a]
endBy1 p sep = some (p >>= \x -> sep >> return x)

manyTill :: (Monad m, Alternative m) => m a -> m end -> m [a]
manyTill p end = go id
  where
    go f = option False (fmap (const True) end) >>= \done ->
      if done then return (f []) else p >>= \x -> go (f . (x :))

someTill :: (Monad m, Alternative m) => m a -> m end -> m [a]
someTill p end = p >>= \x -> fmap (x :) (manyTill p end)

skipMany :: (Monad m, Alternative m) => m a -> m ()
skipMany p = option False (fmap (const True) p) >>= \more -> when more (skipMany p)

skipSome :: (Monad m, Alternative m) => m a -> m ()
skipSome p = p >> skipMany p

skipCount :: Monad m => Int -> m a -> m ()
skipCount n p = if n <= 0 then return () else p >> skipCount (n - 1) p

skipManyTill :: (Monad m, Alternative m) => m a -> m end -> m end
skipManyTill p end = end <|> (p >> skipManyTill p end)
