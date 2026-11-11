module Data.Time.Clock where

-- A moment: nanoseconds since the start of 1970, in UTC. Upstream counts
-- days and picoseconds; this is enough to compare moments and to show
-- them as upstream does.

newtype UTCTime = UTCTime Integer
  deriving (Eq, Ord)

-- 2026-11-11 13:15:00.123456789 UTC
instance Show UTCTime where
  show time = case civil time of
    (year, month, day, hour, minute, second, nanoseconds) ->
      show year ++ "-" ++ two month ++ "-" ++ two day ++ " " ++ two hour ++ ":" ++ two minute ++ ":" ++ two second
        ++ fraction nanoseconds
        ++ " UTC"
    where
      fraction nanoseconds =
        if nanoseconds == 0
          then ""
          else '.' : reverse (dropWhile (== '0') (reverse (pad 9 (show nanoseconds))))

two :: Integer -> String
two n = pad 2 (show n)

pad :: Int -> String -> String
pad width digits = replicate (width - length digits) '0' ++ digits

-- The year, month, day, hour, minute, second and nanoseconds of a moment.
civil :: UTCTime -> (Integer, Integer, Integer, Integer, Integer, Integer, Integer)
civil (UTCTime nanoseconds) =
  let (seconds, fraction) = divMod nanoseconds 1000000000
      (days, inDay) = divMod seconds 86400
      -- Days to a date, in the calendar that repeats every 400 years
      -- (Howard Hinnant's algorithm), with years that start in March.
      shifted = days + 719468
      era = div shifted 146097
      dayOfEra = shifted - era * 146097
      yearOfEra = div (dayOfEra - div dayOfEra 1460 + div dayOfEra 36524 - div dayOfEra 146096) 365
      dayOfYear = dayOfEra - (365 * yearOfEra + div yearOfEra 4 - div yearOfEra 100)
      shiftedMonth = div (5 * dayOfYear + 2) 153
      day = dayOfYear - div (153 * shiftedMonth + 2) 5 + 1
      month = if shiftedMonth < 10 then shiftedMonth + 3 else shiftedMonth - 9
      year = yearOfEra + era * 400 + (if month <= 2 then 1 else 0)
   in (year, month, day, div inDay 3600, mod (div inDay 60) 60, mod inDay 60, fraction)

-- A length of time, in nanoseconds. A number literal is seconds.
newtype NominalDiffTime = NominalDiffTime Integer
  deriving (Eq, Ord)

instance Num NominalDiffTime where
  (+) (NominalDiffTime a) (NominalDiffTime b) = NominalDiffTime (a + b)
  (-) (NominalDiffTime a) (NominalDiffTime b) = NominalDiffTime (a - b)
  (*) (NominalDiffTime a) (NominalDiffTime b) = NominalDiffTime (div (a * b) 1000000000)
  negate (NominalDiffTime a) = NominalDiffTime (negate a)
  abs (NominalDiffTime a) = NominalDiffTime (abs a)
  signum (NominalDiffTime a) = NominalDiffTime (signum a * 1000000000)
  fromInteger seconds = NominalDiffTime (seconds * 1000000000)

diffUTCTime :: UTCTime -> UTCTime -> NominalDiffTime
diffUTCTime (UTCTime a) (UTCTime b) = NominalDiffTime (a - b)

addUTCTime :: NominalDiffTime -> UTCTime -> UTCTime
addUTCTime (NominalDiffTime d) (UTCTime t) = UTCTime (t + d)
