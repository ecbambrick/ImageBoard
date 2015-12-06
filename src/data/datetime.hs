module Data.DateTime 
    ( TimeZone, DateTime, defaultFormatDate, getCurrentTime, getCurrentTimeZone
    , fromSeconds, toSeconds, utcTimeZone ) where

import Data.Time             ( UTCTime, getCurrentTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Time.Format      ( formatTime )
import Data.Time.LocalTime   ( TimeZone, getCurrentTimeZone, utc, utcToLocalTime )
import System.Locale         ( defaultTimeLocale )

-- | Represents a date and time as UTC time.
type DateTime = UTCTime

-- | Formats the given date time and time zone using a default format.
defaultFormatDate :: TimeZone -> DateTime -> String
defaultFormatDate timeZone dateTime = 
    let localTime = utcToLocalTime timeZone dateTime
    in  formatTime defaultTimeLocale "%F" localTime

-- | Converts an integer to a date time.
fromSeconds :: Integer -> DateTime
fromSeconds = posixSecondsToUTCTime . fromInteger

-- | Converts a date time to an integer.
toSeconds :: DateTime -> Integer 
toSeconds = round . utcTimeToPOSIXSeconds

-- | The UTC time zone.
utcTimeZone :: TimeZone
utcTimeZone = utc
