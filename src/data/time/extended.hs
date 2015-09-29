module Data.Time.Extended ( UTCTime, fromSeconds, toSeconds ) where

import Data.Time             ( UTCTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )

-- | Converts an integer to a UTC time.
fromSeconds :: Integer -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger

-- | Converts a UTC time to an integer.
toSeconds :: UTCTime -> Integer 
toSeconds = round . utcTimeToPOSIXSeconds
