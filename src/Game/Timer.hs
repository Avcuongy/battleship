{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Timer
    ( -- * Types
      TimerConfig(..)
    , TimerState(..)
    
    -- * Timer Operations
    , startTimer
    , isTimerExpired
    , getRemainingTime
    , pauseTimer
    , resumeTimer
    , resetTimer
    
    -- * Helpers
    , defaultTimerConfig
    , formatTime
    ) where

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime, NominalDiffTime)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Control.Monad (when)

-- ===== TYPES =====

-- | Timer configuration
data TimerConfig = TimerConfig
    { turnDuration :: !Int  -- ^ Seconds per turn
    } deriving (Show, Eq, Generic)

instance ToJSON TimerConfig where
    toJSON (TimerConfig dur) = object
        [ "turnDuration" .= dur
        ]

instance FromJSON TimerConfig where
    parseJSON = withObject "TimerConfig" $ \v ->
        TimerConfig <$> v .: "turnDuration"

-- | Timer state (tracks current countdown)
data TimerState = TimerState
    { startTime     :: !UTCTime  -- ^ When timer started
    , remainingTime :: !Int      -- ^ Seconds remaining
    , isActive      :: !Bool     -- ^ Whether timer is counting down
    } deriving (Show, Generic)

instance ToJSON TimerState where
    toJSON (TimerState _ remaining active) = object
        [ "remainingTime" .= remaining
        , "isActive"      .= active
        ]

-- Note: FromJSON intentionally not implemented - timer state should not be deserialized

-- ===== TIMER OPERATIONS =====

-- | Start new timer with given duration (seconds)
startTimer :: Int -> IO TimerState
startTimer duration = do
    now <- getCurrentTime
    return $ TimerState
        { startTime = now
        , remainingTime = duration
        , isActive = True
        }

-- | Check if timer has expired
isTimerExpired :: TimerState -> IO Bool
isTimerExpired timer
    | not (isActive timer) = return False
    | otherwise = do
        remaining <- getRemainingTime timer
        return $ remaining <= 0

-- | Get remaining time in seconds
getRemainingTime :: TimerState -> IO Int
getRemainingTime timer
    | not (isActive timer) = return (remainingTime timer)
    | otherwise = do
        now <- getCurrentTime
        let elapsed = floor $ diffUTCTime now (startTime timer)
        return $ max 0 (remainingTime timer - elapsed)

-- | Pause timer (saves remaining time, stops countdown)
pauseTimer :: TimerState -> IO TimerState
pauseTimer timer = do
    remaining <- getRemainingTime timer
    return $ timer 
        { remainingTime = remaining
        , isActive = False
        }

-- | Resume timer with remaining time
resumeTimer :: TimerState -> IO TimerState
resumeTimer timer = do
    now <- getCurrentTime
    return $ timer 
        { startTime = now
        , isActive = True
        }

-- | Reset timer to original duration
resetTimer :: Int -> IO TimerState
resetTimer = startTimer

-- ===== HELPERS =====

-- | Default timer config: 30 seconds per turn
defaultTimerConfig :: TimerConfig
defaultTimerConfig = TimerConfig { turnDuration = 30 }

-- | Format remaining time as "MM:SS"
formatTime :: Int -> Text
formatTime totalSeconds =
    let minutes = totalSeconds `div` 60
        seconds = totalSeconds `mod` 60
    in T.pack $ padZero minutes ++ ":" ++ padZero seconds
  where
    padZero n = if n < 10 then "0" ++ show n else show n
