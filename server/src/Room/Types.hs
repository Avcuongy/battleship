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

instance ToJSON GameMode where
  toJSON AI = String "AI"
  toJSON OneVsOne = String "1vs1"

instance FromJSON GameMode where
  parseJSON = withText "GameMode" $ \t ->
    case t of
      "AI" -> return AI
      "1vs1" -> return OneVsOne
      _ -> fail "Invalid game mode"

-- | Room status
data RoomStatus
  = Ready       -- Players are setting up ships
  | InProgress  -- Game is in progress
  | Done        -- Game finished
  deriving (Show, Eq, Generic)

instance ToJSON RoomStatus where
  toJSON Ready = String "ready"
  toJSON InProgress = String "in_progress"
  toJSON Done = String "done"

instance FromJSON RoomStatus where
  parseJSON = withText "RoomStatus" $ \t ->
    case t of
      "ready" -> return Ready
      "in_progress" -> return InProgress
      "done" -> return Done
      _ -> fail "Invalid room status"

-- | Player info in a room
data PlayerInfo = PlayerInfo
  { playerInfoId :: !PlayerId
  , playerInfoName :: !PlayerName
  , playerInfoReady :: !Bool
  } deriving (Show, Eq, Generic)

instance ToJSON PlayerInfo where
  toJSON p = object
    [ "id" .= playerInfoId p
    , "name" .= playerInfoName p
    , "ready" .= playerInfoReady p
    ]

instance FromJSON PlayerInfo where
  parseJSON = withObject "PlayerInfo" $ \v -> PlayerInfo
    <$> v .: "id"
    <$> v .: "name"
    <$> v .: "ready"

-- | Room data
data Room = Room
  { roomId :: !RoomId
  , gameMode :: !GameMode
  , status :: !RoomStatus
  , player1 :: !PlayerInfo
  , player2 :: !PlayerInfo
  } deriving (Show, Eq, Generic)

instance ToJSON Room where
  toJSON r = object
    [ "roomId" .= roomId r
    , "gameMode" .= gameMode r
    , "status" .= status r
    , "players" .= object
        [ "player1" .= player1 r
        , "player2" .= player2 r
        ]
    ]

instance FromJSON Room where
  parseJSON = withObject "Room" $ \v -> do
    rid <- v .: "roomId"
    gm <- v .: "gameMode"
    st <- v .: "status"
    players <- v .: "players"
    p1 <- players .: "player1"
    p2 <- players .: "player2"
    return $ Room rid gm st p1 p2

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
