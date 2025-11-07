{-# LANGUAGE OverloadedStrings #-}

{-|
Send messages to all players in a room simultaneously.
-}

module Network.WebSocket.Broadcast
    ( broadcastToRoom
    , broadcastGameStart
    , broadcastPlayerReady
    , broadcastAttackResult
    , broadcastGameOver
    , broadcastTimeout
    ) where

import qualified Network.WebSockets as WS
import Control.Concurrent.STM (atomically)
import Data.Text (Text)
import qualified Data.Text as T

import Network.Protocol
import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.Player as PlayerMgr
import Game.Types (Position, Result(..))
import Network.WebSocket.Types (WebSocketState(..))

-- ============================================================================
-- Generic Broadcast
-- ============================================================================

-- | Broadcast message to all players in room
broadcastToRoom :: WebSocketState -> Text -> ServerMessage -> IO ()
broadcastToRoom state roomId msg = do
    -- Get player IDs in room
    maybePlayerIds <- RoomMgr.getRoomPlayerIds
        (wsRoomManager state) roomId
    
    case maybePlayerIds of
        Nothing -> return ()
        Just (player1Id, player2Id) -> do
            -- Get connections
            maybeConn1 <- PlayerMgr.getConnection 
                (wsPlayerManager state) player1Id
            maybeConn2 <- PlayerMgr.getConnection 
                (wsPlayerManager state) player2Id
            
            -- Send to both players
            case maybeConn1 of
                Nothing -> return ()
                Just conn1 -> WS.sendTextData conn1 (encodeServerMessage msg)
            
            case maybeConn2 of
                Nothing -> return ()
                Just conn2 -> WS.sendTextData conn2 (encodeServerMessage msg)

-- ============================================================================
-- Specific Broadcasts
-- ============================================================================

-- | Broadcast game start
broadcastGameStart :: WebSocketState -> Text -> IO ()
broadcastGameStart state roomId = do
    putStrLn $ "Broadcasting game start for room: " ++ T.unpack roomId
    let msg = GameStartMsg $ GameStartMessage { gsmRoomId = roomId }
    broadcastToRoom state roomId msg

-- | Broadcast player ready status update
broadcastPlayerReady :: WebSocketState -> Text -> Text -> Bool -> IO ()
broadcastPlayerReady state roomId playerId isReady = do
    putStrLn $ "Broadcasting ready status: " ++ T.unpack playerId ++ " -> " ++ show isReady
    let msg = PlayerReadyMsg $ PlayerReadyMessage { prmPlayerId = playerId, prmReady = isReady }
    broadcastToRoom state roomId msg

-- | Broadcast attack result
broadcastAttackResult :: WebSocketState -> Text -> Text -> Position 
                      -> Result -> IO ()
broadcastAttackResult state roomId attackerId pos result = do
    -- Determine next turn based on result
    nextTurn <- case result of
        ResultMiss -> do
            -- Miss: opponent's turn
            maybeOpponent <- RoomMgr.getOpponentId 
                (wsRoomManager state) roomId attackerId
            return $ maybe attackerId id maybeOpponent
        
        ResultHit -> 
            -- Hit: same player continues
            return attackerId
        
        ResultShipSunk _ -> 
            -- Ship sunk: same player continues
            return attackerId
    
    -- Create message
    let msg = AttackResultMsg $ AttackResultMessage
            { armAttacker = attackerId
            , armPosition = pos
            , armResult = resultToText result
            , armShipType = case result of
                ResultShipSunk sType -> Just (T.pack $ show sType)
                _ -> Nothing
            , armNextTurn = nextTurn
            }
    
    -- Broadcast to both players
    broadcastToRoom state roomId msg

-- | Broadcast game over
broadcastGameOver :: WebSocketState -> Text -> Text -> IO ()
broadcastGameOver state roomId winnerId = do
    -- Get winner name
    maybeWinnerName <- PlayerMgr.getPlayerName 
        (wsPlayerManager state) winnerId
    
    let winnerName = maybe "Unknown" id maybeWinnerName
    
    let msg = GameOverMsg $ GameOverMessage
            { gomWinner = winnerId
            , gomWinnerName = winnerName
            , gomReason = "all_ships_sunk"
            }
    
    putStrLn $ "Game over: winner = " ++ T.unpack winnerName
    broadcastToRoom state roomId msg

-- | Broadcast timeout
broadcastTimeout :: WebSocketState -> Text -> Text -> IO ()
broadcastTimeout state roomId playerId = do
    putStrLn $ "Timeout: " ++ T.unpack playerId
    -- Determine nextTurn from room state
    maybeOpponent <- RoomMgr.getOpponentId (wsRoomManager state) roomId playerId
    case maybeOpponent of
        Nothing -> return ()
        Just nextTurnId -> do
            let msg = TimeoutServerMsg $ TimeoutServerMessage
                        { tsmPlayerId = playerId
                        , tsmNextTurn = nextTurnId
                        }
            broadcastToRoom state roomId msg

-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Convert Result to text
resultToText :: Result -> Text
resultToText ResultMiss = "miss"
resultToText ResultHit = "hit"
resultToText (ResultShipSunk _) = "sunk"
