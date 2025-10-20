{-# LANGUAGE OverloadedStrings #-}

module State.Manager
    ( -- * PvP State
      PvPState(..)
    , initPvPState
    
    -- * Matchmaking
    , joinQueue
    , leaveQueue
    , getQueueSize
    
    -- * Game Management
    , createGame
    , getGame
    , updateGame
    , removeGame
    , getAllGames
    
    -- * Connection Management
    , registerConnection
    , unregisterConnection
    , getConnection
    , broadcastToRoom
    
    -- * Player Management
    , setPlayerShips
    , setPlayerReady
    , processPlayerMove
    
    -- * Queries
    , isRoomFull
    , getActiveRoomCount
    ) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.WebSockets as WS

import State.GameState
import Game.Types
import Game.Board
import Game.Engine (processAttack)

-- ===== PVP STATE (STM) =====

-- | Global PvP state with STM synchronization
data PvPState = PvPState
    { waitingQueue :: !(TVar [Text])                       -- ^ Player IDs waiting for match
    , activeGames  :: !(TVar (M.Map Text GameState))       -- ^ roomId -> GameState
    , connections  :: !(TVar (M.Map Text WS.Connection))   -- ^ playerId -> WebSocket connection
    }

-- | Initialize PvP state
initPvPState :: IO PvPState
initPvPState = do
    queue <- newTVarIO []
    games <- newTVarIO M.empty
    conns <- newTVarIO M.empty
    return $ PvPState queue games conns

-- ===== MATCHMAKING (STM) =====

-- | Join matchmaking queue
-- Returns: Left "waiting" if no opponent found, Right roomId if matched
joinQueue :: PvPState -> Text -> Text -> IO (Either Text Text)
joinQueue state playerId playerName = do
    result <- atomically $ do
        queue <- readTVar (waitingQueue state)
        case queue of
            [] -> do
                -- No one waiting, add to queue
                writeTVar (waitingQueue state) [playerId]
                return $ Left "Waiting for opponent"
            (opponentId:rest) -> do
                -- Found opponent, remove from queue
                writeTVar (waitingQueue state) rest
                return $ Right opponentId
    
    case result of
        Left msg -> return $ Left msg
        Right opponentId -> do
            -- Create game room
            roomId <- generateRoomId
            gameState <- initGameState roomId playerId playerName opponentId "Opponent" 30
            
            -- Add to active games atomically
            atomically $ modifyTVar (activeGames state) $ 
                M.insert roomId gameState
            
            return $ Right roomId

-- | Leave matchmaking queue
leaveQueue :: PvPState -> Text -> STM ()
leaveQueue state playerId = do
    modifyTVar (waitingQueue state) $ filter (/= playerId)

-- | Get current queue size
getQueueSize :: PvPState -> STM Int
getQueueSize state = do
    queue <- readTVar (waitingQueue state)
    return $ length queue

-- ===== GAME MANAGEMENT (STM) =====

-- | Create new game room
createGame :: PvPState -> Text -> GameState -> STM ()
createGame state roomId gameState = do
    modifyTVar (activeGames state) $ M.insert roomId gameState

-- | Get game by room ID
getGame :: PvPState -> Text -> STM (Maybe GameState)
getGame state roomId = do
    games <- readTVar (activeGames state)
    return $ M.lookup roomId games

-- | Update game state atomically
updateGame :: PvPState -> Text -> (GameState -> GameState) -> STM ()
updateGame state roomId updateFn = do
    modifyTVar (activeGames state) $ M.adjust updateFn roomId

-- | Remove game room
removeGame :: PvPState -> Text -> STM ()
removeGame state roomId = do
    modifyTVar (activeGames state) $ M.delete roomId

-- | Get all active games
getAllGames :: PvPState -> STM (M.Map Text GameState)
getAllGames state = readTVar (activeGames state)

-- ===== CONNECTION MANAGEMENT (STM) =====

-- | Register WebSocket connection for player
registerConnection :: PvPState -> Text -> WS.Connection -> STM ()
registerConnection state playerId conn = do
    modifyTVar (connections state) $ M.insert playerId conn

-- | Unregister connection
unregisterConnection :: PvPState -> Text -> STM ()
unregisterConnection state playerId = do
    modifyTVar (connections state) $ M.delete playerId

-- | Get connection for player
getConnection :: PvPState -> Text -> STM (Maybe WS.Connection)
getConnection state playerId = do
    conns <- readTVar (connections state)
    return $ M.lookup playerId conns

-- | Broadcast message to all players in a room
broadcastToRoom :: PvPState -> Text -> Text -> IO ()
broadcastToRoom state roomId message = do
    (mGame, conns) <- atomically $ do
        games <- readTVar (activeGames state)
        cs <- readTVar (connections state)
        return (M.lookup roomId games, cs)
    
    case mGame of
        Just game -> do
            let p1Id = playerId (player1 game)
            let p2Id = playerId (player2 game)
            
            -- Send to both players
            mapM_ (sendToPlayer conns message) [p1Id, p2Id]
        Nothing -> return ()
  where
    sendToPlayer conns msg pid =
        case M.lookup pid conns of
            Just conn -> WS.sendTextData conn msg
            Nothing -> return ()

-- ===== PLAYER MANAGEMENT (STM) =====

-- | Set player ships (setup phase)
setPlayerShips :: PvPState -> Text -> Text -> [Ship] -> STM (Either Text ())
setPlayerShips state roomId playerId ships = do
    mGame <- getGame state roomId
    case mGame of
        Nothing -> return $ Left "Room not found"
        Just game -> do
            let p1 = player1 game
            let p2 = player2 game
            
            if playerId == State.GameState.playerId p1
                then do
                    let newP1 = p1 
                            { State.GameState.ships = ships
                            , State.GameState.board = initBoardWithShips ships
                            }
                    updateGame state roomId $ \g -> g { player1 = newP1 }
                    return $ Right ()
                else if playerId == State.GameState.playerId p2
                    then do
                        let newP2 = p2 
                                { State.GameState.ships = ships
                                , State.GameState.board = initBoardWithShips ships
                                }
                        updateGame state roomId $ \g -> g { player2 = newP2 }
                        return $ Right ()
                    else return $ Left "Player not in room"

-- | Set player ready status
setPlayerReady :: PvPState -> Text -> Text -> Bool -> STM ()
setPlayerReady state roomId playerId ready = do
    updateGame state roomId $ \game ->
        let p1 = player1 game
            p2 = player2 game
        in if playerId == State.GameState.playerId p1
            then game { player1 = setPlayerReady p1 ready }
            else if playerId == State.GameState.playerId p2
                then game { player2 = setPlayerReady p2 ready }
                else game

-- | Process player move atomically
processPlayerMove :: PvPState -> Text -> Text -> Position -> STM (Either Text MoveResult)
processPlayerMove state roomId playerId pos = do
    mGame <- getGame state roomId
    case mGame of
        Nothing -> return $ Left "Room not found"
        Just game -> do
            -- Check if it's player's turn
            if not (isPlayerTurn game playerId)
                then return $ Left "Not your turn"
                else do
                    -- Determine attacker and defender
                    let (attacker, defender) = 
                            if playerId == State.GameState.playerId (player1 game)
                                then (player1 game, player2 game)
                                else (player2 game, player1 game)
                    
                    -- Process attack
                    let (newDefenderState, result) = processAttack 
                            (toPlayerState defender) pos
                    
                    -- Update game state
                    let newDefender = updatePlayerFromState defender newDefenderState
                    let updatedGame = if playerId == State.GameState.playerId (player1 game)
                            then game { player2 = newDefender }
                            else game { player1 = newDefender }
                    
                    updateGame state roomId (const $ updateGameState updatedGame playerId pos result)
                    return $ Right result
  where
    toPlayerState p = Game.Engine.PlayerState
        { Game.Engine.playerShips = State.GameState.ships p
        , Game.Engine.playerBoard = State.GameState.board p
        , Game.Engine.opponentTrackBoard = State.GameState.attackBoard p
        , Game.Engine.totalMoves = State.GameState.totalMoves p
        , Game.Engine.shipsRemaining = State.GameState.shipsLeft p
        }
    
    updatePlayerFromState p newState = p
        { State.GameState.board = Game.Engine.playerBoard newState
        , State.GameState.shipsLeft = Game.Engine.shipsRemaining newState
        }

-- ===== QUERIES =====

-- | Check if room is full (2 players)
isRoomFull :: GameState -> Bool
isRoomFull game =
    not (T.null $ State.GameState.playerId $ player2 game)

-- | Get number of active rooms
getActiveRoomCount :: PvPState -> STM Int
getActiveRoomCount state = do
    games <- readTVar (activeGames state)
    return $ M.size games

-- ===== HELPERS =====

-- | Generate random room ID (6 characters)
generateRoomId :: IO Text
generateRoomId = do
    -- Simple implementation - in production use crypto-random
    let chars = ['a'..'z'] ++ ['A'..'Z']
    randomChars <- sequence $ replicate 6 $ do
        idx <- randomRIO (0, length chars - 1)
        return $ chars !! idx
    return $ T.pack randomChars
