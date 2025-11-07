{-# LANGUAGE OverloadedStrings #-}

module Network.WebSocket.Handler
    ( handleConnection
    ) where

import qualified Network.WebSockets as WS
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Monad (forever, when)
import Control.Exception (catch, SomeException, fromException)
import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), decode)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import qualified Network.WebSocket.Broadcast as Broadcast
import Network.Protocol
import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.Player as PlayerMgr
import qualified Game.Rules as Rules
import qualified Game.Board as Board
import Game.Types (Position, Result(..))
import qualified Game.Timer as Timer
import Network.WebSocket.Types (WebSocketState(..))
import State.Types (GameStatus(..))
import qualified State.Types as ST

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
    putStrLn $ "=== [WS] Starting connection handler ==="
    putStrLn $ "    RoomId: " ++ T.unpack roomId
    putStrLn $ "    PlayerId: " ++ T.unpack playerId
    
    -- Ensure player exists in PlayerManager (add if missing, derive name from room if possible)
    exists <- PlayerMgr.playerExists (wsPlayerManager state) playerId
    if not exists
        then do
            maybeRoom <- RoomMgr.getRoomState (wsRoomManager state) roomId
            let derivedName = case maybeRoom of
                    Nothing -> "Guest-" <> playerId
                    Just room ->
                        if playerId == ST.player1Id room
                            then ST.player1Name room
                            else maybe ("Guest-" <> playerId) id (ST.player2Name room)
            PlayerMgr.addPlayer (wsPlayerManager state) playerId derivedName
            putStrLn $ "    Player added: " ++ T.unpack playerId ++ " (" ++ T.unpack derivedName ++ ")"
        else
            return ()

    -- Register connection in state
    PlayerMgr.addConnection (wsPlayerManager state) playerId conn
    putStrLn $ "    Connection registered for player: " ++ T.unpack playerId
    
    -- ★ Main message loop (runs in separate thread per connection)
    putStrLn $ "    Starting message loop..."
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
    case () of
        _ | isClientPing msgData ->
              -- Ignore client keepalive pings sent as text frames
              return ()
          | otherwise ->
              case decodeClientMessage msgData of
                  Nothing -> do
                      -- Invalid message: silent ignore (as per requirements)
                      putStrLn $ "Invalid message from " ++ T.unpack playerId
                      return ()
                  
                  Just (ReadyMsg readyMsg) -> 
                      handleReady state conn roomId playerId readyMsg
                  
                  Just (AttackMsg attackMsg) ->
                      handleAttack state conn roomId playerId attackMsg
                  
                  Just (StartMsg startMsg) ->
                      handleStart state conn roomId playerId startMsg

                  Just (TimeoutClientMsg tmsg) ->
                      handleTimeoutClient state conn roomId playerId tmsg

-- | Detect client keepalive ping messages in JSON form: {"type":"ping"}
isClientPing :: BL.ByteString -> Bool
isClientPing bs =
    case decode bs :: Maybe Value of
        Just (Object o) ->
            case KM.lookup (K.fromString "type") o of
                Just (String t) -> t == "ping"
                _ -> False
        _ -> False
-- ============================================================================
-- Start Handler
-- ============================================================================

handleStart :: WebSocketState -> WS.Connection -> Text -> Text 
            -> StartMessage -> IO ()
handleStart state _conn roomId playerId _startMsg = do
    -- Only host (player1) can start
    maybeRoom <- RoomMgr.getRoomState (wsRoomManager state) roomId
    case maybeRoom of
        Nothing -> return ()
        Just room -> do
            if playerId == ST.player1Id room
                then Broadcast.broadcastGameStart state roomId
                else return ()

            return ()

-- ============================================================================
-- Timeout (client-initiated) Handler (This is new)
-- ============================================================================

handleTimeoutClient :: WebSocketState -> WS.Connection -> Text -> Text
                    -> TimeoutClientMessage -> IO ()
handleTimeoutClient state _conn roomId playerId _tmsg = do
    -- Advance turn to opponent and notify both players
    RoomMgr.nextTurn (wsRoomManager state) roomId
    Broadcast.broadcastTimeout state roomId playerId

-- ============================================================================
-- Ready Handler
-- ============================================================================

handleReady :: WebSocketState -> WS.Connection -> Text -> Text 
            -> ReadyMessage -> IO ()
handleReady state conn roomId playerId readyMsg = do
    putStrLn $ "Player ready: " ++ T.unpack playerId
    
    -- Mark player as ready in STM state
    result <- RoomMgr.markPlayerReady 
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
            -- Immediately broadcast this player's ready status
            Broadcast.broadcastPlayerReady state roomId playerId True

            -- Check if both players are ready
            bothReady <- RoomMgr.checkBothReady 
                (wsRoomManager state) roomId
            
            when bothReady $ do
                -- PARALLEL VALIDATION (from Game.Rules)
                maybeFleets <- RoomMgr.getBothFleets 
                    (wsRoomManager state) roomId
                
                case maybeFleets of
                    Nothing -> return ()
                    Just (fleet1, fleet2) -> do
                        -- Validate both fleets in parallel
                        (valid1, valid2) <- Rules.validateBothPlayers fleet1 fleet2
                        
                        if valid1 && valid2
                            then do
                                -- Both valid: start game
                                RoomMgr.setRoomStatus 
                                    (wsRoomManager state) roomId InProgress
                                
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
    maybeTurnStart <- RoomMgr.getTurnStartTime 
        (wsRoomManager state) roomId
    
    case maybeTurnStart of
        Nothing -> return ()  -- Room not found
        Just turnStart -> do
            if Timer.checkTimeout turnStart now
                then do
                    -- Timeout: next player's turn
                    putStrLn $ "Timeout for " ++ T.unpack playerId
                    RoomMgr.nextTurn (wsRoomManager state) roomId
                    Broadcast.broadcastTimeout state roomId playerId
                else do
                    -- Valid: process attack
                    processAttack state roomId playerId (amPosition attackMsg)

-- | Process valid attack
processAttack :: WebSocketState -> Text -> Text -> Position -> IO ()
processAttack state roomId playerId pos = do
    -- Get opponent board from room state
    maybeBoard <- RoomMgr.getOpponentBoard 
        (wsRoomManager state) roomId playerId
    
    case maybeBoard of
        Nothing -> return ()
        Just board -> do
            -- Process attack on board
            let (result, newBoard) = Board.processAttack pos board
            
            -- Update board in state
            RoomMgr.updateOpponentBoard 
                (wsRoomManager state) roomId playerId newBoard
            
            -- Check win condition
            let gameOver = Board.allShipsSunk newBoard
            
            if gameOver
                then do
                    -- Game over: broadcast winner
                    RoomMgr.setRoomStatus 
                        (wsRoomManager state) roomId Done
                    Broadcast.broadcastGameOver state roomId playerId
                else do
                    -- Game continues
                    case result of
                        ResultMiss -> do
                            -- Miss: next player's turn
                            RoomMgr.nextTurn 
                                (wsRoomManager state) roomId
                        
                        ResultHit -> do
                            -- Hit: same player continues (no timer reset)
                            return ()
                        
                        ResultShipSunk _ -> do
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
    putStrLn "=== [WS] Player disconnected ==="
    putStrLn $ "    PlayerId: " ++ T.unpack playerId
    putStrLn $ "    RoomId: " ++ T.unpack roomId
    case fromException ex of
        Just (WS.CloseRequest code msg) ->
            putStrLn $ "    CloseRequest: code=" ++ show code
                     ++ ", reason=" ++ show msg
        Just WS.ConnectionClosed ->
            putStrLn "    ConnectionClosed (no close frame)"
        Just (WS.UnicodeException err) ->
            putStrLn $ "    UnicodeException: " ++ show err
        Just (WS.ParseException err) ->
            putStrLn $ "    ParseException: " ++ err
        Nothing ->
            putStrLn $ "    Error: " ++ show ex
    
    -- Remove connection from state
    PlayerMgr.removeConnection (wsPlayerManager state) playerId

    -- Grace period to allow client to navigate and reconnect (e.g., page change)
    -- If the player does not reconnect within this window, end the game.
    let graceMicros = 8 * 1000 * 1000  -- 8 seconds
    _ <- forkIO $ do
        threadDelay graceMicros
        -- Check if player reconnected
        maybeConn <- PlayerMgr.getConnection (wsPlayerManager state) playerId
        case maybeConn of
            Just _ -> do
                putStrLn $ "    Reconnected within grace period: " ++ T.unpack playerId
                return ()
            Nothing -> do
                putStrLn $ "    No reconnect; finalizing game over for: " ++ T.unpack playerId
                -- Set room status to done and notify opponent
                RoomMgr.setRoomStatus (wsRoomManager state) roomId Done
                maybeOpponent <- RoomMgr.getOpponentId (wsRoomManager state) roomId playerId
                case maybeOpponent of
                    Nothing -> return ()
                    Just opponentId -> Broadcast.broadcastGameOver state roomId opponentId
        return ()
    return ()
