{-# LANGUAGE OverloadedStrings #-}

{-|
Save/load player statistics to JSON files.
Format: data/players/<playerId>.json

When to save:
  * On beforeunload (browser close tab)
  * After game completion (update stats)

When NOT to save:
  * Disconnect mid-game (stats not updated)
-}

module Storage.Player
    ( -- * Types
      PlayerStats(..)
    , PlayerData(..)
    
    -- * Operations
    , savePlayer
    , loadPlayer
    , asyncSavePlayer
    
    -- * Utilities
    , getPlayerFilePath
    ) where

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import qualified Storage.FileIO as FileIO

-- ============================================================================
-- Types
-- ============================================================================

-- | Player statistics
data PlayerStats = PlayerStats
    { gamesPlayed :: !Int
    , wins :: !Int
    , losses :: !Int
    } deriving (Show, Eq, Generic)

instance FromJSON PlayerStats
instance ToJSON PlayerStats

-- | Complete player data (for JSON serialization)
data PlayerData = PlayerData
    { playerDataName :: !Text
    , playerDataStats :: !PlayerStats
    } deriving (Show, Eq, Generic)

instance FromJSON PlayerData where
    parseJSON = \v -> do
        name <- parseJSON v >>= (.: "name")
        stats <- parseJSON v >>= (.: "stats")
        return $ PlayerData name stats

instance ToJSON PlayerData where
    toJSON pd = object
        [ "name" .= playerDataName pd
        , "stats" .= playerDataStats pd
        ]

-- Need these imports for JSON
import Data.Aeson (object, (.:), (.=), parseJSON)
import Control.Applicative ((<$>), (<*>))

-- ============================================================================
-- File Path
-- ============================================================================

-- | Get file path for player ID
-- Format: data/players/<playerId>.json
getPlayerFilePath :: Text -> FilePath
getPlayerFilePath playerId = 
    "data/players/" ++ T.unpack playerId ++ ".json"

-- ============================================================================
-- Save Operations
-- ============================================================================

{-|
Synchronously save player data.
Used when immediate persistence is required.

Example:
>>> savePlayer "abc123" "Player1" (PlayerStats 10 6 4)
-}
savePlayer :: Text -> Text -> PlayerStats -> IO ()
savePlayer playerId playerName stats = do
    let playerData = PlayerData
            { playerDataName = playerName
            , playerDataStats = stats
            }
    
    let path = getPlayerFilePath playerId
    let jsonData = encode playerData
    
    FileIO.syncSave path jsonData
    putStrLn $ "Saved player: " ++ T.unpack playerId

{-|
Asynchronously save player data (non-blocking).
Used for beforeunload events (navigator.sendBeacon).

Example:
>>> asyncSavePlayer "abc123" "Player1" (PlayerStats 10 6 4)
>>> -- Returns immediately, save happens in background
-}
asyncSavePlayer :: Text -> Text -> PlayerStats -> IO ()
asyncSavePlayer playerId playerName stats = do
    let playerData = PlayerData
            { playerDataName = playerName
            , playerDataStats = stats
            }
    
    let path = getPlayerFilePath playerId
    let jsonData = encode playerData
    
    FileIO.asyncSave path jsonData

-- For API handler (from SavePlayerRequest)
asyncSavePlayerFromRequest :: Text -> Text -> Int -> Int -> Int -> IO ()
asyncSavePlayerFromRequest playerId playerName played w l = do
    let stats = PlayerStats
            { gamesPlayed = played
            , wins = w
            , losses = l
            }
    asyncSavePlayer playerId playerName stats

-- ============================================================================
-- Load Operations
-- ============================================================================

{-|
Load player data from file.
Returns Nothing if file doesn't exist or parse error.

Example:
>>> maybePlayer <- loadPlayer "abc123"
>>> case maybePlayer of
>>>     Just (name, stats) -> print stats
>>>     Nothing -> putStrLn "Player not found"
-}
loadPlayer :: Text -> IO (Maybe (Text, PlayerStats))
loadPlayer playerId = do
    let path = getPlayerFilePath playerId
    
    maybeContent <- FileIO.syncLoad path
    
    case maybeContent of
        Nothing -> return Nothing
        Just content -> do
            case decode content of
                Nothing -> do
                    putStrLn $ "Failed to parse player file: " ++ path
                    return Nothing
                Just playerData -> 
                    return $ Just (playerDataName playerData, playerDataStats playerData)

-- ============================================================================
-- Example JSON Format
-- ============================================================================

{-
File: data/players/abc123.json

{
  "name": "Player1",
  "stats": {
    "gamesPlayed": 15,
    "wins": 8,
    "losses": 7
  }
}
-}
