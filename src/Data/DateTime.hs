module Data.DateTime
    ( TimeZone, DateTime, addDays, defaultFormatDate, dropTime, getCurrentTime
    , getCurrentTimeZone, fromDate, fromSeconds, toSeconds, utcTimeZone ) where

import qualified Data.Time.Calendar as Calendar

import Data.Time             ( UTCTime(..), getCurrentTime, secondsToDiffTime )
import Data.Time.Clock.POSIX ( posixSecondsToUTCTime, utcTimeToPOSIXSeconds )
import Data.Time.Format      ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime   ( TimeZone, getCurrentTimeZone, utc, utcToLocalTime )

-- | Represents a date and time as UTC time.
type DateTime = UTCTime

-- | Formats the given date time and time zone using a default format.
defaultFormatDate :: TimeZone -> DateTime -> String
defaultFormatDate timeZone dateTime =
    let localTime = utcToLocalTime timeZone dateTime
    in  formatTime defaultTimeLocale "%F" localTime

-- | Adds the given number of days to the given date time. Negative numbers
-- | will result in days being subtracted.
addDays :: DateTime -> Integer -> DateTime
addDays (UTCTime date time) days = UTCTime (Calendar.addDays days date) time

-- | Returns the given date time without the time component.
dropTime :: DateTime -> DateTime
dropTime (UTCTime date time) = UTCTime date (secondsToDiffTime 0)

-- | Converts a year, month, and day to a date time.
fromDate :: Integer -> Int -> Int -> DateTime
fromDate year month day =
    let date = Calendar.fromGregorian year month day
        time = secondsToDiffTime 0
    in UTCTime date time

-- | Converts an integer to a date time.
fromSeconds :: Integer -> DateTime
fromSeconds = posixSecondsToUTCTime . fromInteger

-- | Converts a date time to an integer.
toSeconds :: DateTime -> Integer
toSeconds = round . utcTimeToPOSIXSeconds

-- | The UTC time zone.
utcTimeZone :: TimeZone
utcTimeZone = utc
