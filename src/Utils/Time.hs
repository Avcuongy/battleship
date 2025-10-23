{-# LANGUAGE OverloadedStrings #-}

{-|
Time operations for game timer validation and timestamps.
-}

module Utils.Time
    ( -- * Types
      Timestamp
    
    -- * Current time
    , getCurrentTimestamp
    , getCurrentTimeText
    
    -- * Time differences
    , timeDiff
    , timeDiffMillis
    
    -- * Formatting
    , formatTimestamp
    , formatISO8601
    
    -- * Parsing
    , parseTimestamp
    
    -- * Utilities
    , addSeconds
    , addMinutes
    ) where

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, addUTCTime)
import Data.Time.Format (formatTime, parseTimeM, defaultTimeLocale)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Text (Text)
import qualified Data.Text as T

-- ============================================================================
-- Types
-- ============================================================================

-- | Timestamp type alias (uses UTCTime from time package)
type Timestamp = UTCTime

-- ============================================================================
-- Current Time
-- ============================================================================

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = getCurrentTime
{-# INLINE getCurrentTimestamp #-}

getCurrentTimeText :: IO Text
getCurrentTimeText = do
    now <- getCurrentTime
    return $ formatISO8601 now

-- ============================================================================
-- Time Differences
-- ============================================================================
    
-- | Difference in seconds
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff start end = floor $ diffUTCTime end start
{-# INLINE timeDiff #-}


timeDiffMillis :: Timestamp -> Timestamp -> Int
timeDiffMillis start end = 
    floor $ (diffUTCTime end start) * 1000

-- ============================================================================
-- Formatting
-- ============================================================================

formatTimestamp :: Timestamp -> String
formatTimestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"

formatISO8601 :: Timestamp -> Text
formatISO8601 = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- ============================================================================
-- Parsing
-- ============================================================================

parseTimestamp :: String -> Maybe Timestamp
parseTimestamp = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

-- ============================================================================
-- Time Arithmetic
-- ============================================================================

addSeconds :: Int -> Timestamp -> Timestamp
addSeconds secs time = addUTCTime (fromIntegral secs) time

addMinutes :: Int -> Timestamp -> Timestamp
addMinutes mins time = addUTCTime (fromIntegral mins * 60) time

-- ============================================================================
-- POSIX Conversions (for timestamps in JSON)
-- ============================================================================

timestampToPOSIX :: Timestamp -> Double
timestampToPOSIX = realToFrac . utcTimeToPOSIXSeconds

posixToTimestamp :: Double -> Timestamp
posixToTimestamp = posixSecondsToUTCTime . realToFrac

-- ============================================================================
-- Helper Functions
-- ============================================================================

isInPast :: Timestamp -> IO Bool
isInPast time = do
    now <- getCurrentTimestamp
    return (time < now)

isInFuture :: Timestamp -> IO Bool
isInFuture time = do
    now <- getCurrentTimestamp
    return (time > now)

getAge :: Timestamp -> IO Int
getAge time = do
    now <- getCurrentTimestamp
    return $ timeDiff time now
