{-# LANGUAGE OverloadedStrings #-}

module Network.WebSocket.Server
    ( startWebSocketServer
    , WebSocketState(..)
    ) where

import qualified Network.WebSockets as WS
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai (defaultApp)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.STM (TVar)
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)

import qualified Network.WebSocket.Handler as Handler
import State.Manager.Room (RoomManager)
import State.Manager.Player (PlayerManager)

-- ============================================================================
-- Types
-- ============================================================================

-- | Global WebSocket state (connections map)
data WebSocketState = WebSocketState
    { wsRoomManager :: RoomManager
    , wsPlayerManager :: PlayerManager
    , wsConnections :: TVar (Map T.Text WS.Connection)  -- playerId -> connection
    }

-- ============================================================================
-- Server Setup
-- ============================================================================

{-|
Start WebSocket server on port 9160.

URL format: ws://localhost:9160?roomId=ABC123&playerId=xyz789

Flow:
  1. Client connects with roomId and playerId in query params
  2. Server accepts connection
  3. Spawn handler thread (forkIO) for this connection
  4. Handler processes messages until disconnect
-}
startWebSocketServer :: WebSocketState -> IO ()
startWebSocketServer state = do
    putStrLn "WebSocket server starting on port 9160..."
    run 9160 $ websocketsOr
        WS.defaultConnectionOptions
        (wsApp state)
        defaultApp

-- | WebSocket application
wsApp :: WebSocketState -> WS.ServerApp
wsApp state pending = do
    -- Accept WebSocket connection
    conn <- WS.acceptRequest pending
    
    -- Enable ping/pong for connection keepalive
    WS.withPingThread conn 30 (return ()) $ do
        -- Extract query parameters from request
        let requestPath = WS.requestPath (WS.pendingRequest pending)
        case parseQueryParams requestPath of
            Nothing -> do
                -- Invalid params: close connection
                WS.sendTextData conn ("Error: Missing roomId or playerId" :: T.Text)
                WS.sendClose conn ("Invalid connection params" :: T.Text)
            
            Just (roomId, playerId) -> do
                putStrLn $ "New connection: roomId=" ++ T.unpack roomId 
                         ++ ", playerId=" ++ T.unpack playerId
                
                -- ★ CONCURRENCY: Handle connection in separate thread
                Handler.handleConnection state conn roomId playerId

-- ============================================================================
-- Query Parameter Parsing
-- ============================================================================

-- | Parse query params from request path
-- Expected: /?roomId=ABC123&playerId=xyz789
parseQueryParams :: WS.RequestHead -> Maybe (T.Text, T.Text)
parseQueryParams req =
    let path = T.pack $ WS.requestPath req
        query = T.pack $ WS.requestQuery req
    in parseQuery query
  where
    parseQuery :: T.Text -> Maybe (T.Text, T.Text)
    parseQuery q =
        let params = map (T.breakOn "=") (T.splitOn "&" q)
            lookup' key = fmap (T.drop 1 . snd) $ 
                         find ((== key) . fst) params
        in case (lookup' "roomId", lookup' "playerId") of
            (Just rid, Just pid) -> Just (rid, pid)
            _ -> Nothing
    
    find _ [] = Nothing
    find pred (x:xs)
        | pred x = Just x
        | otherwise = find pred xs
