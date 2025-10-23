{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    ( PlayerStats(..)
    , PlayerData(..)
    , savePlayer
    , loadPlayer
    , asyncSavePlayer
    , asyncSavePlayerFromRequest
    , getPlayerFilePath
    ) where

import Data.Aeson (FromJSON, ToJSON, encode, decode, withObject, (.:), (.=), object)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Exception (catch, SomeException)

import qualified Storage.FileIO as FileIO

-- ============================================================================
-- Types
-- ============================================================================

data PlayerStats = PlayerStats
    { gamesPlayed :: !Int
    , wins :: !Int
    , losses :: !Int
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

data PlayerData = PlayerData
    { playerDataName :: !Text
    , playerDataStats :: !PlayerStats
    } deriving (Show, Eq, Generic, FromJSON, ToJSON)

-- ============================================================================
-- File Path
-- ============================================================================

getPlayerFilePath :: Text -> FilePath
getPlayerFilePath pid = "data/players/" ++ T.unpack pid ++ ".json"

-- ============================================================================
-- Save/Load Operations
-- ============================================================================

savePlayer :: Text -> Text -> PlayerStats -> IO ()
savePlayer pid name stats = do
    let pdata = PlayerData name stats
        path = getPlayerFilePath pid
    FileIO.syncSave path (encode pdata)
        `catch` \(e :: SomeException) -> putStrLn $ "Save failed: " ++ show e

asyncSavePlayer :: Text -> Text -> PlayerStats -> IO ()
asyncSavePlayer pid name stats = do
    let pdata = PlayerData name stats
        path = getPlayerFilePath pid
    FileIO.asyncSave path (encode pdata)

asyncSavePlayerFromRequest :: Text -> Text -> Int -> Int -> Int -> IO ()
asyncSavePlayerFromRequest pid name gp w l =
    asyncSavePlayer pid name (PlayerStats gp w l)

loadPlayer :: Text -> IO (Maybe (Text, PlayerStats))
loadPlayer pid = do
    let path = getPlayerFilePath pid
    maybeBytes <- FileIO.syncLoad path
    case maybeBytes >>= decode of
        Nothing -> do
            putStrLn $ "Failed to load player: " ++ T.unpack pid
            return Nothing
        Just pd -> return $ Just (playerDataName pd, playerDataStats pd)
