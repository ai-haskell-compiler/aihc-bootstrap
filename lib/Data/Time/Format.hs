module Data.Time.Format where

import Data.Time.Clock (UTCTime, civil, pad)

data TimeLocale = TimeLocale

defaultTimeLocale :: TimeLocale
defaultTimeLocale = TimeLocale

-- The moment written as the format says. Of upstream's directives, these
-- are known: %Y %m %d %H %M %S and %%.
formatTime :: TimeLocale -> String -> UTCTime -> String
formatTime _ format time = case civil time of
  (year, month, day, hour, minute, second, _) -> go format
    where
      go text = case text of
        '%' : c : rest -> directive c ++ go rest
        c : rest -> c : go rest
        [] -> []
      directive c = case c of
        'Y' -> show year
        'm' -> pad 2 (show month)
        'd' -> pad 2 (show day)
        'H' -> pad 2 (show hour)
        'M' -> pad 2 (show minute)
        'S' -> pad 2 (show second)
        '%' -> "%"
        _ -> interpreterTodo ("Data.Time.Format.formatTime: %" ++ [c])

foreign import prim "interpreterTodo" interpreterTodo :: String -> a
