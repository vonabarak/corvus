-- | Conversion between 'UTCTime' and the POSIX-nanoseconds 'Int64'
-- representation used on the Cap'n Proto wire. The schema documents
-- @0@ as the "absent" sentinel for optional timestamps.
module Corvus.Wire.Time
  ( utcTimeToNanos
  , nanosToUtcTime
  , utcTimeToNanosMaybe
  , nanosToUtcTimeMaybe
  )
where

import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

-- | POSIX nanoseconds since the epoch.
utcTimeToNanos :: UTCTime -> Int64
utcTimeToNanos t =
  let posix = utcTimeToPOSIXSeconds t
   in floor (realToFrac posix * 1e9 :: Double)

nanosToUtcTime :: Int64 -> UTCTime
nanosToUtcTime n =
  let secs = fromIntegral n / 1e9 :: Double
   in posixSecondsToUTCTime (realToFrac secs)

-- | Optional timestamp helpers. The schema uses @0@ as the absent
-- sentinel; 'Nothing' on the Haskell side maps to it.
utcTimeToNanosMaybe :: Maybe UTCTime -> Int64
utcTimeToNanosMaybe = maybe 0 utcTimeToNanos

nanosToUtcTimeMaybe :: Int64 -> Maybe UTCTime
nanosToUtcTimeMaybe 0 = Nothing
nanosToUtcTimeMaybe n = Just (nanosToUtcTime n)
