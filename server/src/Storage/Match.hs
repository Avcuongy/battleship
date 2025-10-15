{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage.Match
  ( -- * Types
    Match(..)
  , MatchState(..)
  , RoomId
  , GameMode(..)
  , RoomStatus(..)
    -- * Operations
  , saveMatch
  , saveMatchAsync
  , loadMatch
  , finalizeMatch
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Concurrent (forkIO)
import Storage.Player (PlayerId, updatePlayerStats)


type RoomId = Text  -- 6 characters [a-zA-Z]

-- Game mode: "1vs1" or "ai"
data GameMode = OneVsOne | AI
  deriving (Show, Eq, Generic)

instance ToJSON GameMode where
  toJSON OneVsOne = "1vs1"
  toJSON AI = "ai"

instance FromJSON GameMode where
  parseJSON = withText "GameMode" $ \case
    "1vs1" -> pure OneVsOne
    "ai" -> pure AI
    _ -> fail "Invalid game mode"

-- Room status: ready, in_progress, done
data RoomStatus = Ready | InProgress | Done
  deriving (Show, Eq, Generic)

instance ToJSON RoomStatus where
  toJSON Ready = "ready"
  toJSON InProgress = "in_progress"
  toJSON Done = "done"

instance FromJSON RoomStatus where
  parseJSON = withText "RoomStatus" $ \case
    "ready" -> pure Ready
    "in_progress" -> pure InProgress
    "done" -> pure Done
    _ -> fail "Invalid room status"

-- Match state containing winner and loser IDs
data MatchState = MatchState
  { id_player_win :: PlayerId
  , id_player_lose :: PlayerId
  } deriving (Show, Eq, Generic)

instance ToJSON MatchState where
  toJSON (MatchState win lose) = object
    [ "id_player_win" .= win
    , "id_player_lose" .= lose
    ]

instance FromJSON MatchState where
  parseJSON = withObject "MatchState" $ \v -> MatchState
    <$> v .: "id_player_win"
    <*> v .: "id_player_lose"

-- Complete match record
data Match = Match
  { gameMode :: GameMode
  , status :: RoomStatus
  , state :: MatchState
  } deriving (Show, Eq, Generic)

-- Custom JSON instance to match your structure
instance ToJSON Match where
  toJSON (Match mode stat matchState) = object
    [ "gameMode" .= mode
    , "status" .= stat
    , "state" .= matchState
    ]

instance FromJSON Match where
  parseJSON = withObject "Match" $ \v -> Match
    <$> v .: "gameMode"
    <*> v .: "status"
    <*> v .: "state"

-- ============ OPERATIONS ============

-- Save match to data/matches/<room_id>.json
saveMatch :: RoomId -> Match -> IO ()
saveMatch roomId match = do
  createDirectoryIfMissing True "data/matches"
  let filepath = "data/matches/" <> T.unpack roomId <> ".json"
  BL.writeFile filepath (encode match)

-- [PARALLEL] Save match asynchronously (non-blocking)
saveMatchAsync :: RoomId -> Match -> IO ()
saveMatchAsync roomId match = do
  _ <- forkIO $ saveMatch roomId match
  return ()

-- Load match from data/matches/<room_id>.json
loadMatch :: RoomId -> IO (Maybe Match)
loadMatch roomId = do
  let filepath = "data/matches/" <> T.unpack roomId <> ".json"
  exists <- doesFileExist filepath
  if exists
    then decode <$> BL.readFile filepath
    else return Nothing

-- Finalize match: save match and update player stats
-- This is the main function to call when a game ends
finalizeMatch :: RoomId -> GameMode -> PlayerId -> PlayerId -> IO ()
finalizeMatch roomId mode winnerId loserId = do
  let match = Match
        { gameMode = mode
        , status = Done
        , state = MatchState winnerId loserId
        }
  
  -- Save match asynchronously (parallel)
  saveMatchAsync roomId match
  
  -- Update both players' stats
  updatePlayerStats winnerId True   -- Winner
  updatePlayerStats loserId False   -- Loser
