{-# LANGUAGE OverloadedStrings #-}

module Room.Manager
  ( RoomMap
  , newRoomMap
  , createRoom
  , getRoom
  , updateRoom
  , deleteRoom
  , listRooms
  , generateRoomId
  ) where

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, writeTVar, modifyTVar', atomically)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)

import Room.Types (Room, RoomId, GameMode, newRoom, roomId)
import Player.Types (PlayerId, PlayerName)

-- | Thread-safe map of rooms
type RoomMap = TVar (Map RoomId Room)

-- | Create a new empty room map
newRoomMap :: IO RoomMap
newRoomMap = atomically $ newTVar Map.empty

-- | Generate a random 6-character room ID
generateRoomId :: IO RoomId
generateRoomId = do
  chars <- sequence $ replicate 6 randomChar
  return $ T.pack chars
  where
    randomChar = do
      let letters = ['a'..'z'] ++ ['A'..'Z']
      idx <- randomRIO (0, length letters - 1)
      return $ letters !! idx

-- | Create a new room
createRoom :: RoomMap -> GameMode -> PlayerId -> PlayerName -> IO (Either String Room)
createRoom roomMap mode pid pname = do
  rid <- generateRoomId
  let room = newRoom rid mode pid pname
  atomically $ do
    rooms <- readTVar roomMap
    if Map.member rid rooms
      then return $ Left "Room ID collision, please try again"
      else do
        writeTVar roomMap (Map.insert rid room rooms)
        return $ Right room

-- | Get a room by ID
getRoom :: RoomMap -> RoomId -> IO (Maybe Room)
getRoom roomMap rid = atomically $ do
  rooms <- readTVar roomMap
  return $ Map.lookup rid rooms

-- | Update a room
updateRoom :: RoomMap -> Room -> IO ()
updateRoom roomMap room = atomically $ do
  modifyTVar' roomMap (Map.insert (roomId room) room)

-- | Delete a room
deleteRoom :: RoomMap -> RoomId -> IO ()
deleteRoom roomMap rid = atomically $ do
  modifyTVar' roomMap (Map.delete rid)

-- | List all rooms
listRooms :: RoomMap -> IO [Room]
listRooms roomMap = atomically $ do
  rooms <- readTVar roomMap
  return $ Map.elems rooms
