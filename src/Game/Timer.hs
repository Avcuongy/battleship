{-# LANGUAGE OverloadedStrings #-}

module Game.Timer
    ( -- * Types
      Timestamp
    
    -- * Timer operations
    , getCurrentTimestamp
    , checkTimeout
    , timeDiff
    
    -- * Constants
    , turnTimeLimit
    ) where

import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

-- ============================================================================
-- Types
-- ============================================================================

type Timestamp = UTCTime

-- ============================================================================
-- Constants
-- ============================================================================

-- | Turn time limit in seconds
turnTimeLimit :: Int
turnTimeLimit = 20  -- Can be adjusted as needed

-- ============================================================================
-- Timer Operations
-- ============================================================================

-- | Get current timestamp
getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = getCurrentTime

-- | Check if turn has timed out (> 20 seconds)
checkTimeout :: Timestamp -> Timestamp -> Bool
checkTimeout turnStart attackTime =
    timeDiff turnStart attackTime > turnTimeLimit

-- | Get time difference in seconds
timeDiff :: Timestamp -> Timestamp -> Int
timeDiff start end = 
    floor $ diffUTCTime end start

-- ============================================================================
-- Example
-- ============================================================================

{-
-- In WebSocket Handler when attack received:

handleAttack :: RoomId -> PlayerId -> Position -> IO ()
handleAttack roomId playerId pos = do
    -- Get turn start time from room state
    turnStart <- getTurnStartTime roomId
    
    -- Check timeout
    now <- getCurrentTimestamp
    if checkTimeout turnStart now
        then do
            -- Timeout: skip this attack, next player's turn
            broadcastTimeout roomId playerId
        else do
            -- Valid: process attack
            result <- processAttack pos roomId
            broadcastResult roomId result
-}
