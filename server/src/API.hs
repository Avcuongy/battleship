{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API
  ( -- * API Types
    LoginRequest(..)
  , LoginResponse(..)
  , CreateRoomRequest(..)
  , CreateRoomResponse(..)
  , SetReadyRequest(..)
  , GameResult(..)
  
  -- * API Handlers
  , handleLogin
  , handleCreateRoom
  , handleSetReady
  , handleGameComplete
  ) where

import Data.Aeson (FromJSON, ToJSON, object, (.=))
import Data.Text (Text, pack)
import GHC.Generics (Generic)

import qualified Player.Manager as PM
import qualified Player.Types as PT
import qualified Room.Manager as RM
import qualified Room.Types as RT
import qualified Storage.Match as SM
import qualified Storage.Player as SP

-- | Login request from client
data LoginRequest = LoginRequest
  { loginName :: !Text
  } deriving (Show, Generic)

instance FromJSON LoginRequest
instance ToJSON LoginRequest

-- | Login response to client
data LoginResponse = LoginResponse
  { playerId :: !Text
  , playerName :: !Text
  , success :: !Bool
  , message :: !Text
  } deriving (Show, Generic)

instance ToJSON LoginResponse

-- | Create room request
data CreateRoomRequest = CreateRoomRequest
  { createPlayerId :: !Text
  , gameMode :: !Text  -- "AI" or "1vs1"
  } deriving (Show, Generic)

instance FromJSON CreateRoomRequest
instance ToJSON CreateRoomRequest

-- | Create room response
data CreateRoomResponse = CreateRoomResponse
  { roomId :: !Text
  , roomCreated :: !Bool
  , roomMessage :: !Text
  } deriving (Show, Generic)

instance ToJSON CreateRoomResponse

-- | Set ready request
data SetReadyRequest = SetReadyRequest
  { readyRoomId :: !Text
  , readyPlayerId :: !Text
  , isReady :: !Bool
  } deriving (Show, Generic)

instance FromJSON SetReadyRequest
instance ToJSON SetReadyRequest

-- | Game completion result
data GameResult = GameResult
  { gameRoomId :: !Text
  , winnerId :: !Text
  , loserId :: !Text
  } deriving (Show, Generic)

instance FromJSON GameResult
instance ToJSON GameResult

-- | Handle player login
handleLogin :: LoginRequest -> IO LoginResponse
handleLogin req = do
  -- Generate a simple player ID (in production, use UUID)
  let pid = "player_" <> loginName req
  
  -- Check if player exists
  result <- PM.getPlayer pid
  
  case result of
    Right player -> return $ LoginResponse
      { playerId = PT.playerId player
      , playerName = PT.playerName player
      , success = True
      , message = "Login successful"
      }
    Left _ -> do
      -- Create new player
      createResult <- PM.createPlayer pid (loginName req)
      case createResult of
        Right player -> return $ LoginResponse
          { playerId = PT.playerId player
          , playerName = PT.playerName player
          , success = True
          , message = "New player created"
          }
        Left err -> return $ LoginResponse
          { playerId = ""
          , playerName = ""
          , success = False
          , message = Data.Text.pack err
          }

-- | Handle room creation
handleCreateRoom :: RM.RoomMap -> CreateRoomRequest -> IO CreateRoomResponse
handleCreateRoom roomMap req = do
  playerResult <- PM.getPlayer (createPlayerId req)
  
  case playerResult of
    Left err -> return $ CreateRoomResponse
      { roomId = ""
      , roomCreated = False
      , roomMessage = Data.Text.pack err
      }
    Right player -> do
      let mode = case gameMode req of
                   "AI" -> RT.AI
                   _ -> RT.OneVsOne
      
      roomResult <- RM.createRoom roomMap mode 
                      (PT.playerId player) 
                      (PT.playerName player)
      
      case roomResult of
        Right room -> return $ CreateRoomResponse
          { roomId = RT.roomId room
          , roomCreated = True
          , roomMessage = "Room created successfully"
          }
        Left err -> return $ CreateRoomResponse
          { roomId = ""
          , roomCreated = False
          , roomMessage = pack err
          }

-- | Handle player ready status
handleSetReady :: RM.RoomMap -> SetReadyRequest -> IO Bool
handleSetReady roomMap req = do
  maybeRoom <- RM.getRoom roomMap (readyRoomId req)
  
  case maybeRoom of
    Nothing -> return False
    Just room -> do
      let updatedRoom = RT.setPlayerReady (readyPlayerId req) (isReady req) room
      RM.updateRoom roomMap updatedRoom
      return True

-- | Handle game completion
handleGameComplete :: RM.RoomMap -> GameResult -> IO Bool
handleGameComplete roomMap result = do
  maybeRoom <- RM.getRoom roomMap (gameRoomId result)
  
  case maybeRoom of
    Nothing -> return False
    Just room -> do
      -- Update player stats
      _ <- PM.recordGameResult (winnerId result) True
      _ <- PM.recordGameResult (loserId result) False
      
      -- Save match result
      let match = SM.MatchResult
            { SM.matchRoomId = gameRoomId result
            , SM.matchGameMode = RT.gameMode room
            , SM.matchStatus = RT.Done
            , SM.winnerId = winnerId result
            , SM.loserId = loserId result
            }
      
      _ <- SM.saveMatch match
      
      -- Clean up room
      RM.deleteRoom roomMap (gameRoomId result)
      
      return True
