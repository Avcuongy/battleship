{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Room.Types
  ( RoomId
  , GameMode(..)
  , RoomStatus(..)
  , PlayerInfo(..)
  , Room(..)
  , newRoom
  , setPlayerReady
  , bothPlayersReady
  ) where

import Data.Aeson (FromJSON, ToJSON, Value(String), object, (.=), (.:), withObject, withText)
import Data.Text (Text)
import GHC.Generics (Generic)
import Player.Types (PlayerId, PlayerName)

-- | Room unique identifier (6 characters a-z, A-Z)
type RoomId = Text

-- | Game mode
data GameMode 
  = AI       -- Single player vs AI
  | OneVsOne -- Two players
  deriving (Show, Eq, Generic)

instance ToJSON GameMode
instance FromJSON GameMode

-- | Room status
data RoomStatus
  = Ready       -- Players are setting up ships
  | InProgress  -- Game is in progress
  | Done        -- Game finished
  deriving (Show, Eq, Generic)

instance ToJSON RoomStatus
instance FromJSON RoomStatus

-- | Player info in a room
data PlayerInfo = PlayerInfo
  { playerInfoId :: !PlayerId
  , playerInfoName :: !PlayerName
  , playerInfoReady :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PlayerInfo
instance FromJSON PlayerInfo

-- | Room data
data Room = Room
  { roomId :: !RoomId
  , gameMode :: !GameMode
  , status :: !RoomStatus
  , player1 :: !PlayerInfo
  , player2 :: !PlayerInfo
  } deriving (Show, Eq, Generic)

instance ToJSON Room
instance FromJSON Room

-- | Create a new room
newRoom :: RoomId -> GameMode -> PlayerId -> PlayerName -> Room
newRoom rid mode p1Id p1Name = Room
  { roomId = rid
  , gameMode = mode
  , status = Ready
  , player1 = PlayerInfo p1Id p1Name False
  , player2 = case mode of
      AI -> PlayerInfo "AI" "AI" True
      OneVsOne -> PlayerInfo "" "" False
  }

-- | Set a player as ready
setPlayerReady :: PlayerId -> Bool -> Room -> Room
setPlayerReady pid ready room
  | playerInfoId (player1 room) == pid = room { player1 = (player1 room) { playerInfoReady = ready } }
  | playerInfoId (player2 room) == pid = room { player2 = (player2 room) { playerInfoReady = ready } }
  | otherwise = room

-- | Check if both players are ready
bothPlayersReady :: Room -> Bool
bothPlayersReady room = 
  playerInfoReady (player1 room) && playerInfoReady (player2 room)
