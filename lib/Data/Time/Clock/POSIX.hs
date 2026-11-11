module Data.Time.Clock.POSIX where

import Data.Time.Clock (NominalDiffTime (..), UTCTime (..))

-- Time since the start of 1970.
type POSIXTime = NominalDiffTime

posixSecondsToUTCTime :: POSIXTime -> UTCTime
posixSecondsToUTCTime (NominalDiffTime nanoseconds) = UTCTime nanoseconds

utcTimeToPOSIXSeconds :: UTCTime -> POSIXTime
utcTimeToPOSIXSeconds (UTCTime nanoseconds) = NominalDiffTime nanoseconds
