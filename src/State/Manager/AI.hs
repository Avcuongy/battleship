{-# LANGUAGE OverloadedStrings #-}

{-|
Manages AI game sessions (in-memory only, no file save).
Limitation: Only 1 active AI session at a time.
-}

module State.Manager.AI
    ( -- * Types
      AIManager
    
    -- * Initialization
    , newAIManager
    , generateUniqueGameId
    
    -- * Session operations
    , createAISession
    , getAISession
    , updateAISession
    , deleteAISession
    
    -- * Board accessors
    , playerBoard
    , aiBoard
    ) where

import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Time.Clock (getCurrentTime)

import State.Types
import State.Sync
import Game.Types (Board, Fleet)
import qualified Game.Board as Board
import qualified Game.AI as AI
import qualified Utils.Random as Random

-- ============================================================================
-- Types
-- ============================================================================

-- | AI session manager
newtype AIManager = AIManager
    { aiSessionsVar :: TVar (Map GameId AIGameState)
    }

-- ============================================================================
-- Initialization
-- ============================================================================

-- | Create new AI manager
newAIManager :: IO AIManager
newAIManager = do
    var <- newTVarIO Map.empty
    return $ AIManager var

-- | Generate unique game ID
generateUniqueGameId :: IO GameId
generateUniqueGameId = Random.randomId 8

-- ============================================================================
-- Session Operations
-- ============================================================================

-- | Create new AI session (only if no active sessions)
createAISession :: AIManager -> GameId -> PlayerId -> Text -> Fleet -> IO (Either Text ())
createAISession mgr gameId playerId playerName playerFleet = do
    now <- getCurrentTime
    
    atomically $ do
        sessions <- readTVar (aiSessionsVar mgr)
        
        -- Check limit: only 1 active session
        if not (Map.null sessions)
            then return $ Left "Another AI session is active"
            else do
                let newSession = AIGameState
                        { aiGameId = gameId
                        , aiPlayerId = playerId
                        , aiPlayerName = playerName
                        , aiPlayerBoard = Board.createBoard playerFleet
                        , aiOpponentBoard = Board.createBoard AI.aiDefaultFleet
                        , aiCurrentTurn = "player"
                        , aiCreatedAt = now
                        }
                
                writeTVar (aiSessionsVar mgr) (Map.insert gameId newSession sessions)
                return $ Right ()

-- | Get AI session
getAISession :: AIManager -> GameId -> IO (Maybe AIGameState)
getAISession mgr gameId = atomicLookup gameId (aiSessionsVar mgr)

-- | Update AI session boards
updateAISession :: AIManager -> GameId -> Board -> Board -> IO ()
updateAISession mgr gameId newPlayerBoard newAIBoard = do
    atomically $ do
        sessions <- readTVar (aiSessionsVar mgr)
        case Map.lookup gameId sessions of
            Nothing -> return ()
            Just session -> do
                let updated = session
                        { aiPlayerBoard = newPlayerBoard
                        , aiOpponentBoard = newAIBoard
                        }
                writeTVar (aiSessionsVar mgr) (Map.insert gameId updated sessions)

-- | Delete AI session (game over)
deleteAISession :: AIManager -> GameId -> IO ()
deleteAISession mgr gameId = do
    atomicModify (aiSessionsVar mgr) (Map.delete gameId)

-- ============================================================================
-- Board Accessors
-- ============================================================================

-- | Get player board from session
playerBoard :: AIGameState -> Board
playerBoard = aiPlayerBoard

-- | Get AI board from session
aiBoard :: AIGameState -> Board
aiBoard = aiOpponentBoard
