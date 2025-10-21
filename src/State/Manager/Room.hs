{-# LANGUAGE OverloadedStrings #-}

{-|
Manages 1vs1 game rooms using STM for thread-safe concurrent access.
Limitation: Only 1 active room at a time (as per requirements).
-}

module State.Manager.Room
    ( -- * Types
      RoomManager
    
    -- * Initialization
    , newRoomManager
    , generateUniqueRoomId
    
    -- * Room operations
    , createRoom
    , joinRoom
    , getRoomState
    , deleteRoom
    
    -- * Player operations
    , markPlayerReady
    , checkBothReady
    , getBothFleets
    
    -- * Game operations
    , setRoomStatus
    , nextTurn
    , getTurnStartTime
    , getOpponentId
    , getOpponentBoard
    , updateOpponentBoard
    
    -- * Query operations
    , getRoomPlayerIds
    ) where

import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Time.Clock (getCurrentTime, UTCTime)

import State.Types
import State.Sync
import Game.Types (Fleet, Board, Position)
import qualified Game.Board as Board
import qualified Utils.Random as Random

-- ============================================================================
-- Types
-- ============================================================================

-- | Room manager: TVar holding map of rooms
-- Limit: Only 1 active room (enforced by createRoom)
newtype RoomManager = RoomManager
    { roomsVar :: TVar (Map RoomId RoomState)
    }

-- ============================================================================
-- Initialization
-- ============================================================================

-- | Create new room manager
newRoomManager :: IO RoomManager
newRoomManager = do
    var <- newTVarIO Map.empty
    return $ RoomManager var

-- | Generate unique 6-char room ID (case-sensitive)
generateUniqueRoomId :: IO RoomId
generateUniqueRoomId = Random.randomId 6

-- ============================================================================
-- Room Operations
-- ============================================================================

-- | Create new room (only if no active rooms exist)
createRoom :: RoomManager -> RoomId -> PlayerId -> Text -> IO (Either Text RoomState)
createRoom mgr roomId playerId playerName = do
    now <- getCurrentTime
    
    atomically $ do
        rooms <- readTVar (roomsVar mgr)
        
        -- Check if any active rooms exist (limit: 1 room)
        let activeRooms = Map.filter (\r -> status r /= Done) rooms
        
        if not (Map.null activeRooms)
            then return $ Left "Another room is already active"
            else do
                let newRoom = RoomState
                        { roomId = roomId
                        , gameMode = "1vs1"
                        , status = Ready
                        , player1Id = playerId
                        , player1Name = playerName
                        , player1Ready = False
                        , player1Fleet = Nothing
                        , player1Board = Nothing
                        , player2Id = Nothing
                        , player2Name = Nothing
                        , player2Ready = False
                        , player2Fleet = Nothing
                        , player2Board = Nothing
                        , currentTurn = Nothing
                        , turnStartTime = Nothing
                        , createdAt = now
                        }
                
                writeTVar (roomsVar mgr) (Map.insert roomId newRoom rooms)
                return $ Right newRoom

-- | Join existing room as player 2
joinRoom :: RoomManager -> RoomId -> PlayerId -> Text -> IO (Either Text ())
joinRoom mgr roomId playerId playerName = do
    atomically $ do
        rooms <- readTVar (roomsVar mgr)
        
        case Map.lookup roomId rooms of
            Nothing -> return $ Left "Room not found"
            Just room -> do
                -- Check if room is full
                case player2Id room of
                    Just _ -> return $ Left "Room is full"
                    Nothing -> do
                        let updatedRoom = room
                                { player2Id = Just playerId
                                , player2Name = Just playerName
                                }
                        writeTVar (roomsVar mgr) (Map.insert roomId updatedRoom rooms)
                        return $ Right ()

-- | Get room state
getRoomState :: RoomManager -> RoomId -> IO (Maybe RoomState)
getRoomState mgr roomId = atomicLookup roomId (roomsVar mgr)

-- | Delete room
deleteRoom :: RoomManager -> RoomId -> IO ()
deleteRoom mgr roomId = do
    atomicModify (roomsVar mgr) (Map.delete roomId)

-- ============================================================================
-- Player Operations
-- ============================================================================

-- | Mark player as ready with fleet
markPlayerReady :: RoomManager -> RoomId -> PlayerId -> Fleet -> IO (Either Text ())
markPlayerReady mgr roomId playerId fleet = do
    atomically $ do
        rooms <- readTVar (roomsVar mgr)
        
        case Map.lookup roomId rooms of
            Nothing -> return $ Left "Room not found"
            Just room -> do
                let updatedRoom
                        | playerId == player1Id room =
                            room { player1Ready = True
                                 , player1Fleet = Just fleet
                                 , player1Board = Just (Board.createBoard fleet)
                                 }
                        | Just playerId == player2Id room =
                            room { player2Ready = True
                                 , player2Fleet = Just fleet
                                 , player2Board = Just (Board.createBoard fleet)
                                 }
                        | otherwise = room
                
                writeTVar (roomsVar mgr) (Map.insert roomId updatedRoom rooms)
                return $ Right ()

-- | Check if both players are ready
checkBothReady :: RoomManager -> RoomId -> IO Bool
checkBothReady mgr roomId = do
    maybeRoom <- getRoomState mgr roomId
    return $ case maybeRoom of
        Nothing -> False
        Just room -> player1Ready room && player2Ready room

-- | Get both fleets (for parallel validation)
getBothFleets :: RoomManager -> RoomId -> IO (Maybe (Fleet, Fleet))
getBothFleets mgr roomId = do
    maybeRoom <- getRoomState mgr roomId
    return $ do
        room <- maybeRoom
        fleet1 <- player1Fleet room
        fleet2 <- player2Fleet room
        return (fleet1, fleet2)

-- ============================================================================
-- Game Operations
-- ============================================================================

-- | Set room status
setRoomStatus :: RoomManager -> RoomId -> GameStatus -> IO ()
setRoomStatus mgr roomId newStatus = do
    now <- getCurrentTime
    atomically $ do
        rooms <- readTVar (roomsVar mgr)
        case Map.lookup roomId rooms of
            Nothing -> return ()
            Just room -> do
                let updatedRoom = room { status = newStatus }
                -- If starting game, set initial turn and time
                let finalRoom = if newStatus == InProgress
                        then updatedRoom 
                            { currentTurn = Just (player1Id room)
                            , turnStartTime = Just now
                            }
                        else updatedRoom
                
                writeTVar (roomsVar mgr) (Map.insert roomId finalRoom rooms)

-- | Switch to next player's turn
nextTurn :: RoomManager -> RoomId -> IO ()
nextTurn mgr roomId = do
    now <- getCurrentTime
    atomically $ do
        rooms <- readTVar (roomsVar mgr)
        case Map.lookup roomId rooms of
            Nothing -> return ()
            Just room -> do
                let nextPlayerId = case currentTurn room of
                        Just pid | pid == player1Id room -> player2Id room
                        _ -> Just (player1Id room)
                
                let updatedRoom = room
                        { currentTurn = nextPlayerId
                        , turnStartTime = Just now
                        }
                
                writeTVar (roomsVar mgr) (Map.insert roomId updatedRoom rooms)

-- | Get turn start time
getTurnStartTime :: RoomManager -> RoomId -> IO (Maybe UTCTime)
getTurnStartTime mgr roomId = do
    maybeRoom <- getRoomState mgr roomId
    return $ maybeRoom >>= turnStartTime

-- | Get opponent's player ID
getOpponentId :: RoomManager -> RoomId -> PlayerId -> IO (Maybe PlayerId)
getOpponentId mgr roomId playerId = do
    maybeRoom <- getRoomState mgr roomId
    return $ do
        room <- maybeRoom
        if playerId == player1Id room
            then player2Id room
            else Just (player1Id room)

-- | Get opponent's board
getOpponentBoard :: RoomManager -> RoomId -> PlayerId -> IO (Maybe Board)
getOpponentBoard mgr roomId playerId = do
    maybeRoom <- getRoomState mgr roomId
    return $ do
        room <- maybeRoom
        if playerId == player1Id room
            then player2Board room
            else player1Board room

-- | Update opponent's board after attack
updateOpponentBoard :: RoomManager -> RoomId -> PlayerId -> Board -> IO ()
updateOpponentBoard mgr roomId attackerId newBoard = do
    atomically $ do
        rooms <- readTVar (roomsVar mgr)
        case Map.lookup roomId rooms of
            Nothing -> return ()
            Just room -> do
                let updatedRoom
                        | attackerId == player1Id room =
                            room { player2Board = Just newBoard }
                        | Just attackerId == player2Id room =
                            room { player1Board = Just newBoard }
                        | otherwise = room
                
                writeTVar (roomsVar mgr) (Map.insert roomId updatedRoom rooms)

-- ============================================================================
-- Query Operations
-- ============================================================================

-- | Get both player IDs in room
getRoomPlayerIds :: RoomManager -> RoomId -> IO (Maybe (PlayerId, PlayerId))
getRoomPlayerIds mgr roomId = do
    maybeRoom <- getRoomState mgr roomId
    return $ do
        room <- maybeRoom
        p2 <- player2Id room
        return (player1Id room, p2)
