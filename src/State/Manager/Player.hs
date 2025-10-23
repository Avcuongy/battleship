{-# LANGUAGE OverloadedStrings #-}

{-|
Manages player connections and metadata using STM.
Includes collision detection for player IDs.
-}

module State.Manager.Player
    ( -- * Types
      PlayerManager
    
    -- * Initialization
    , newPlayerManager
    , generateUniquePlayerId
    
    -- * Player operations
    , addPlayer
    , getPlayer
    , removePlayer
    
    -- * Connection management
    , addConnection
    , removeConnection
    , getConnection
    
    -- * Query operations
    , getPlayerName
    , playerExists
    ) where

import Control.Concurrent.STM 
    (TVar, newTVarIO, atomically, readTVar, writeTVar, modifyTVar')
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Network.WebSockets as WS

import State.Types
import State.Sync
import qualified Utils.Random as Random

-- ============================================================================
-- Types
-- ============================================================================

-- | Player manager with ID collision protection
data PlayerManager = PlayerManager
    { playersVar :: TVar (Map PlayerId PlayerInfo)
    , usedIdsVar :: TVar (Set PlayerId)
    }

-- ============================================================================
-- Initialization
-- ============================================================================

-- | Create new player manager
newPlayerManager :: IO PlayerManager
newPlayerManager = do
    players <- newTVarIO Map.empty
    usedIds <- newTVarIO Set.empty
    return $ PlayerManager players usedIds

-- | Generate unique player ID (6 chars, retry if collision)
generateUniquePlayerId :: PlayerManager -> IO PlayerId
generateUniquePlayerId mgr = atomically $ do
    ids <- readTVar (usedIdsVar mgr)
    let loop = do
            newId <- Random.randomId 6
            if newId `Set.member` ids
                then loop
                else do
                    let newIds = Set.insert newId ids
                    writeTVar (usedIdsVar mgr) newIds
                    return newId
    loop

-- ============================================================================
-- Player Operations
-- ============================================================================

-- | Add new player safely (replace if same ID already existed)
addPlayer :: PlayerManager -> PlayerId -> Text -> IO ()
addPlayer mgr playerId playerName = atomically $ do
    modifyTVar' (playersVar mgr) $ Map.insert playerId playerInfo
    modifyTVar' (usedIdsVar mgr) $ Set.insert playerId
  where
    playerInfo = PlayerInfo
        { playerId = playerId
        , playerName = playerName
        , currentRoomId = Nothing
        , playerConnection = Nothing
        }

-- | Get player info
getPlayer :: PlayerManager -> PlayerId -> IO (Maybe PlayerInfo)
getPlayer mgr playerId = atomicLookup playerId (playersVar mgr)

-- | Remove player (frees ID as well)
removePlayer :: PlayerManager -> PlayerId -> IO ()
removePlayer mgr playerId = atomically $ do
    modifyTVar' (playersVar mgr) $ Map.delete playerId
    modifyTVar' (usedIdsVar mgr) $ Set.delete playerId

-- ============================================================================
-- Connection Management
-- ============================================================================

-- | Add WebSocket connection to player
addConnection :: PlayerManager -> PlayerId -> WS.Connection -> IO ()
addConnection mgr playerId conn = atomically $ do
    players <- readTVar (playersVar mgr)
    case Map.lookup playerId players of
        Nothing -> return ()
        Just player -> do
            let updated = player { playerConnection = Just conn }
            writeTVar (playersVar mgr) (Map.insert playerId updated players)

-- | Remove WebSocket connection
removeConnection :: PlayerManager -> PlayerId -> IO ()
removeConnection mgr playerId = atomically $ do
    players <- readTVar (playersVar mgr)
    case Map.lookup playerId players of
        Nothing -> return ()
        Just player -> do
            let updated = player { playerConnection = Nothing }
            writeTVar (playersVar mgr) (Map.insert playerId updated players)

-- | Get connection (if exists)
getConnection :: PlayerManager -> PlayerId -> IO (Maybe WS.Connection)
getConnection mgr playerId = do
    fmap playerConnection <$> getPlayer mgr playerId

-- ============================================================================
-- Query Operations
-- ============================================================================

-- | Get player name
getPlayerName :: PlayerManager -> PlayerId -> IO (Maybe Text)
getPlayerName mgr playerId = fmap playerName <$> getPlayer mgr playerId

-- | Check if player exists
playerExists :: PlayerManager -> PlayerId -> IO Bool
playerExists mgr playerId = atomicMember playerId (playersVar mgr)
