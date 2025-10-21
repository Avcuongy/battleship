{-# LANGUAGE OverloadedStrings #-}

module Network.WebSocket.Handler
    ( handleConnection
    ) where

import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Monad (forever, when)
import Control.Exception (catch, SomeException)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import Network.WebSocket.Server (WebSocketState(..))
import qualified Network.WebSocket.Broadcast as Broadcast
import Network.Protocol
import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.Player as PlayerMgr
import qualified Game.Rules as Rules
import qualified Game.Board as Board
import qualified Game.Timer as Timer

-- ============================================================================
-- Connection Handler (CONCURRENCY)
-- ============================================================================

{-|
CONCURRENCY REQUIREMENT

Handle WebSocket connection in a SEPARATE THREAD.
Each player connection gets its own thread via forkIO.

This allows multiple players to connect and play simultaneously
without blocking each other.
-}
handleConnection :: WebSocketState -> WS.Connection -> Text -> Text -> IO ()
handleConnection state conn roomId playerId = do
    -- Register connection in state
    atomically $ PlayerMgr.addConnection (wsPlayerManager state) playerId conn
    
    putStrLn $ "Handling connection for player: " ++ T.unpack playerId
    
    -- ★ Main message loop (runs in separate thread per connection)
    messageLoop state conn roomId playerId
        `catch` handleDisconnect state roomId playerId

-- ============================================================================
-- Message Loop
-- ============================================================================

-- | Receive and process messages until disconnect
messageLoop :: WebSocketState -> WS.Connection -> Text -> Text -> IO ()
messageLoop state conn roomId playerId = forever $ do
    -- Receive message from client
    msgData <- WS.receiveData conn :: IO BL.ByteString
    
    -- Decode message
    case decodeClientMessage msgData of
        Nothing -> do
            -- Invalid message: silent ignore (as per requirements)
            putStrLn $ "Invalid message from " ++ T.unpack playerId
            return ()
        
        Just (ReadyMsg readyMsg) -> 
            handleReady state conn roomId playerId readyMsg
        
        Just (AttackMsg attackMsg) ->
            handleAttack state conn roomId playerId attackMsg

-- ============================================================================
-- Ready Handler
-- ============================================================================

handleReady :: WebSocketState -> WS.Connection -> Text -> Text 
            -> ReadyMessage -> IO ()
handleReady state conn roomId playerId readyMsg = do
    putStrLn $ "Player ready: " ++ T.unpack playerId
    
    -- Mark player as ready in STM state
    result <- atomically $ RoomMgr.markPlayerReady 
        (wsRoomManager state) 
        roomId 
        playerId 
        (rmFleet readyMsg)
    
    case result of
        Left err -> do
            -- Error: send error message
            let errorMsg = ErrorMsg $ ErrorMessage err Nothing
            WS.sendTextData conn (encodeServerMessage errorMsg)
        
        Right _ -> do
            -- Check if both players are ready
            bothReady <- atomically $ RoomMgr.checkBothReady 
                (wsRoomManager state) roomId
            
            when bothReady $ do
                -- ★ PARALLEL VALIDATION (from Game.Rules)
                maybeFleets <- atomically $ RoomMgr.getBothFleets 
                    (wsRoomManager state) roomId
                
                case maybeFleets of
                    Nothing -> return ()
                    Just (fleet1, fleet2) -> do
                        -- Validate both fleets in parallel
                        (valid1, valid2) <- Rules.validateBothPlayers fleet1 fleet2
                        
                        if valid1 && valid2
                            then do
                                -- Both valid: start game
                                atomically $ RoomMgr.setRoomStatus 
                                    (wsRoomManager state) roomId "in_progress"
                                
                                -- Broadcast game start
                                Broadcast.broadcastGameStart state roomId
                            else do
                                -- Invalid fleet(s): send error
                                let errorMsg = ErrorMsg $ ErrorMessage 
                                        "Invalid placement" Nothing
                                Broadcast.broadcastToRoom state roomId errorMsg

-- ============================================================================
-- Attack Handler
-- ============================================================================

handleAttack :: WebSocketState -> WS.Connection -> Text -> Text 
             -> AttackMessage -> IO ()
handleAttack state conn roomId playerId attackMsg = do
    putStrLn $ "Attack from " ++ T.unpack playerId 
             ++ " at " ++ show (amPosition attackMsg)
    
    -- Check timeout (server-side validation)
    now <- Timer.getCurrentTimestamp
    maybeTurnStart <- atomically $ RoomMgr.getTurnStartTime 
        (wsRoomManager state) roomId
    
    case maybeTurnStart of
        Nothing -> return ()  -- Room not found
        Just turnStart -> do
            if Timer.checkTimeout turnStart now
                then do
                    -- Timeout: next player's turn
                    putStrLn $ "Timeout for " ++ T.unpack playerId
                    atomically $ RoomMgr.nextTurn (wsRoomManager state) roomId
                    Broadcast.broadcastTimeout state roomId playerId
                else do
                    -- Valid: process attack
                    processAttack state roomId playerId (amPosition attackMsg)

-- | Process valid attack
processAttack :: WebSocketState -> Text -> Text -> Position -> IO ()
processAttack state roomId playerId pos = do
    -- Get opponent board from room state
    maybeBoard <- atomically $ RoomMgr.getOpponentBoard 
        (wsRoomManager state) roomId playerId
    
    case maybeBoard of
        Nothing -> return ()
        Just board -> do
            -- Process attack on board
            let (result, newBoard) = Board.processAttack pos board
            
            -- Update board in state
            atomically $ RoomMgr.updateOpponentBoard 
                (wsRoomManager state) roomId playerId newBoard
            
            -- Check win condition
            let gameOver = Board.allShipsSunk newBoard
            
            if gameOver
                then do
                    -- Game over: broadcast winner
                    atomically $ RoomMgr.setRoomStatus 
                        (wsRoomManager state) roomId "done"
                    Broadcast.broadcastGameOver state roomId playerId
                else do
                    -- Game continues
                    case result of
                        Board.ResultMiss -> do
                            -- Miss: next player's turn
                            atomically $ RoomMgr.nextTurn 
                                (wsRoomManager state) roomId
                        
                        Board.ResultHit -> do
                            -- Hit: same player continues (no timer reset)
                            return ()
                        
                        Board.ResultShipSunk _ -> do
                            -- Ship sunk: same player continues
                            return ()
                    
                    -- Broadcast result to both players
                    Broadcast.broadcastAttackResult state roomId playerId pos result

-- ============================================================================
-- Disconnect Handler
-- ============================================================================

-- | Handle player disconnect (opponent auto-wins)
handleDisconnect :: WebSocketState -> Text -> Text -> SomeException -> IO ()
handleDisconnect state roomId playerId ex = do
    putStrLn $ "Player disconnected: " ++ T.unpack playerId 
             ++ ", error: " ++ show ex
    
    -- Remove connection from state
    atomically $ PlayerMgr.removeConnection (wsPlayerManager state) playerId
    
    -- Set room status to done (don't save stats as per requirements)
    atomically $ RoomMgr.setRoomStatus (wsRoomManager state) roomId "done"
    
    -- Get opponent ID
    maybeOpponent <- atomically $ RoomMgr.getOpponentId 
        (wsRoomManager state) roomId playerId
    
    case maybeOpponent of
        Nothing -> return ()
        Just opponentId -> do
            -- Broadcast opponent win
            Broadcast.broadcastGameOver state roomId opponentId
