{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

{-|
Save/load room data to JSON files (1vs1 only).
Format: data/rooms/<roomId>.json

When to save:
  * On beforeunload (browser close tab)
  * On normal game completion
  * Option A: Keep files permanently (history)

When NOT to save:
  * AI mode (no room file)
  * Disconnect mid-game (delete room, don't save)
-}

module Storage.Room
    ( -- * Types
      RoomPlayerData(..)
      , RoomData(..)
    
    -- * Operations
    , saveRoom
    , loadRoom
    , asyncSaveRoom
    , deleteRoomFile
    
    -- * Utilities
    , getRoomFilePath
    , roomStateToData
    ) where

import Data.Aeson (FromJSON, ToJSON, encode, decode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (object, (.:), (.=), parseJSON)
import Control.Applicative ((<|>))
import System.Directory (removeFile)
import Control.Exception (catch, SomeException)

import qualified Storage.FileIO as FileIO
import State.Types (RoomState(..), GameStatus(..))

-- ============================================================================
-- Types
-- ============================================================================

-- | Player data in room (for JSON)
data RoomPlayerData = RoomPlayerData
    { rpdId :: !Text
    , rpdName :: !Text
    , rpdReady :: !Bool
    } deriving (Show, Eq, Generic)

instance FromJSON RoomPlayerData
instance ToJSON RoomPlayerData

-- | Room data (for JSON serialization)
data RoomData = RoomData
    { rdGameMode :: !Text            -- "1vs1"
    , rdStatus :: !Text              -- "ready" | "in_progress" | "done"
    , rdPlayer1 :: !RoomPlayerData
    , rdPlayer2 :: !(Maybe RoomPlayerData)
    } deriving (Show, Eq, Generic)

instance FromJSON RoomData
instance ToJSON RoomData

-- ============================================================================
-- File Path
-- ============================================================================

-- | Get file path for room ID (case-sensitive)
-- Format: data/rooms/<roomId>.json
getRoomFilePath :: Text -> FilePath
getRoomFilePath roomId = 
    "data/rooms/" ++ T.unpack roomId ++ ".json"

-- ============================================================================
-- Conversion
-- ============================================================================

-- | Convert RoomState to RoomData (for JSON)
roomStateToData :: RoomState -> RoomData
roomStateToData room = RoomData
    { rdGameMode = gameMode room
    , rdStatus = statusToText (status room)
    , rdPlayer1 = RoomPlayerData
        { rpdId = player1Id room
        , rpdName = player1Name room
        , rpdReady = player1Ready room
        }
    , rdPlayer2 = case player2Id room of
        Nothing -> Nothing
        Just p2Id -> Just $ RoomPlayerData
            { rpdId = p2Id
            , rpdName = maybe "" id (player2Name room)
            , rpdReady = player2Ready room
            }
    }

-- | Convert GameStatus to Text
statusToText :: GameStatus -> Text
statusToText Ready = "ready"
statusToText InProgress = "in_progress"
statusToText Done = "done"

-- ============================================================================
-- Save Operations
-- ============================================================================

{-|
Synchronously save room data.

Example:
>>> saveRoom "ABC123" roomState
-}
saveRoom :: Text -> RoomState -> IO ()
saveRoom roomId roomState = do
    let roomData = roomStateToData roomState
    let path = getRoomFilePath roomId
    let jsonData = encode roomData
    
    FileIO.syncSave path jsonData
    putStrLn $ "Saved room: " ++ T.unpack roomId

{-|
Asynchronously save room data (non-blocking).
Used for beforeunload events.

Example:
>>> asyncSaveRoom "ABC123" roomState
>>> -- Returns immediately
-}
asyncSaveRoom :: Text -> RoomState -> IO ()
asyncSaveRoom roomId roomState = do
    let roomData = roomStateToData roomState
    let path = getRoomFilePath roomId
    let jsonData = encode roomData
    
    FileIO.asyncSave path jsonData

-- ============================================================================
-- Load Operations
-- ============================================================================

{-|
Load room data from file.
Returns Nothing if file doesn't exist or parse error.

Example:
>>> maybeRoom <- loadRoom "ABC123"
-}
loadRoom :: Text -> IO (Maybe RoomData)
loadRoom roomId = do
    let path = getRoomFilePath roomId
    
    maybeContent <- FileIO.syncLoad path
    
    case maybeContent of
        Nothing -> return Nothing
        Just content -> do
            case decode content of
                Nothing -> do
                    putStrLn $ "Failed to parse room file: " ++ path
                    return Nothing
                Just roomData -> 
                    return $ Just roomData

-- ============================================================================
-- Delete Operations
-- ============================================================================

{-|
Delete room file.
Used when player disconnects mid-game (don't save).

Example:
>>> deleteRoomFile "ABC123"
-}
deleteRoomFile :: Text -> IO ()
deleteRoomFile roomId = do
    let path = getRoomFilePath roomId
    
    exists <- FileIO.fileExists path
    if exists
        then do
            removeFile path
                `catch` \(e :: SomeException) -> 
                    putStrLn $ "Error deleting room file " ++ path ++ ": " ++ show e
            putStrLn $ "Deleted room file: " ++ T.unpack roomId
        else
            return ()

-- ============================================================================
-- Example JSON Format
-- ============================================================================

{-
File: data/rooms/ABC123.json

{
  "gameMode": "1vs1",
  "status": "done",
  "player1": {
    "id": "abc123",
    "name": "Player1",
    "ready": true
  },
  "player2": {
    "id": "def456",
    "name": "Player2",
    "ready": true
  }
}
-}
