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

import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
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

-- | Player manager with collision detection
data PlayerManager = PlayerManager
    { playersVar :: TVar (Map PlayerId PlayerInfo)
    , usedIdsVar :: TVar (Set PlayerId)  -- For collision detection
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

-- | Generate unique player ID (6 chars, check collision)
generateUniquePlayerId :: PlayerManager -> IO PlayerId
generateUniquePlayerId mgr = do
    candidate <- Random.randomId 6
    exists <- atomicMember candidate (usedIdsVar mgr)
    
    if exists
        then generateUniquePlayerId mgr  -- Retry
        else do
            -- Mark as used
            atomicModify (usedIdsVar mgr) (Set.insert candidate)
            return candidate

-- ============================================================================
-- Player Operations
-- ============================================================================

-- | Add new player
addPlayer :: PlayerManager -> PlayerId -> Text -> IO ()
addPlayer mgr playerId playerName = do
    let playerInfo = PlayerInfo
            { playerId = playerId
            , playerName = playerName
            , currentRoomId = Nothing
            , playerConnection = Nothing
            }
    
    atomicModify (playersVar mgr) (Map.insert playerId playerInfo)
    atomicModify (usedIdsVar mgr) (Set.insert playerId)

-- | Get player info
getPlayer :: PlayerManager -> PlayerId -> IO (Maybe PlayerInfo)
getPlayer mgr playerId = atomicLookup playerId (playersVar mgr)

-- | Remove player
removePlayer :: PlayerManager -> PlayerId -> IO ()
removePlayer mgr playerId = do
    atomicModify (playersVar mgr) (Map.delete playerId)
    atomicModify (usedIdsVar mgr) (Set.delete playerId)

-- ============================================================================
-- Connection Management
-- ============================================================================

-- | Add WebSocket connection to player
addConnection :: PlayerManager -> PlayerId -> WS.Connection -> IO ()
addConnection mgr playerId conn = do
    atomically $ do
        players <- readTVar (playersVar mgr)
        case Map.lookup playerId players of
            Nothing -> return ()
            Just player -> do
                let updated = player { playerConnection = Just conn }
                writeTVar (playersVar mgr) (Map.insert playerId updated players)

-- | Remove connection (on disconnect)
removeConnection :: PlayerManager -> PlayerId -> IO ()
removeConnection mgr playerId = do
    atomically $ do
        players <- readTVar (playersVar mgr)
        case Map.lookup playerId players of
            Nothing -> return ()
            Just player -> do
                let updated = player { playerConnection = Nothing }
                writeTVar (playersVar mgr) (Map.insert playerId updated players)

-- | Get player connection
getConnection :: PlayerManager -> PlayerId -> IO (Maybe WS.Connection)
getConnection mgr playerId = do
    maybePlayer <- getPlayer mgr playerId
    return $ maybePlayer >>= playerConnection

-- ============================================================================
-- Query Operations
-- ============================================================================

-- | Get player name
getPlayerName :: PlayerManager -> PlayerId -> IO (Maybe Text)
getPlayerName mgr playerId = do
    maybePlayer <- getPlayer mgr playerId
    return $ fmap playerName maybePlayer

-- | Check if player exists
playerExists :: PlayerManager -> PlayerId -> IO Bool
playerExists mgr playerId = atomicMember playerId (playersVar mgr)
