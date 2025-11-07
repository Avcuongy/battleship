{-# LANGUAGE OverloadedStrings #-}

module Network.WebSocket.Server
    ( startWebSocketServer
    , WebSocketState(..)
    ) where

import qualified Network.WebSockets as WS
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status400, parseQuery)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as TE
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, when)

import qualified Network.WebSocket.Handler as Handler
import qualified Network.WebSocket.Broadcast as Broadcast
import Network.WebSocket.Types (WebSocketState(..))
import qualified State.Manager.Room as RoomMgr
import qualified Game.Timer as Timer
import qualified State.Types
import State.Types (GameStatus(..))

-- ======================================================================
-- | Start WebSocket server
startWebSocketServer :: WebSocketState -> IO ()
startWebSocketServer state = do
    putStrLn "WebSocket server running on port 9160"
    -- Fork background timeout monitor
    _ <- forkIO $ timeoutMonitor state
    run 9160 $ websocketsOr
        WS.defaultConnectionOptions
        (wsApp state)
        backupApp

-- ======================================================================
-- | Background thread: monitor active room for timeout every 2 seconds
timeoutMonitor :: WebSocketState -> IO ()
timeoutMonitor state = forever $ do
    threadDelay (2 * 1000 * 1000)  -- 2 seconds
    maybeRoomId <- RoomMgr.activeRoomId (wsRoomManager state)
    case maybeRoomId of
        Nothing -> return ()
        Just roomId -> do
            maybeRoom <- RoomMgr.getRoomState (wsRoomManager state) roomId
            case maybeRoom of
                Nothing -> return ()
                Just room -> do
                    -- Only check timeout if game is in progress
                    when (State.Types.status room == InProgress) $ do
                        case (State.Types.turnStartTime room, State.Types.currentTurn room) of
                            (Just turnStart, Just currentPlayerId) -> do
                                now <- Timer.getCurrentTimestamp
                                when (Timer.checkTimeout turnStart now) $ do
                                    putStrLn $ "[Monitor] Timeout detected for " ++ T.unpack currentPlayerId
                                    -- Advance turn and broadcast
                                    RoomMgr.nextTurn (wsRoomManager state) roomId
                                    Broadcast.broadcastTimeout state roomId currentPlayerId
                            _ -> return ()

-- ======================================================================
-- | Fallback for HTTP requests (non-WS)
backupApp :: Application
backupApp _ respond =
    respond $ responseLBS status400 [] "WebSocket endpoint only."

-- ======================================================================
-- | WebSocket Application
wsApp :: WebSocketState -> WS.ServerApp
wsApp state pending = do
    let req = WS.pendingRequest pending
        path = WS.requestPath req
        hdrs = WS.requestHeaders req
    putStrLn $ "[WS] Incoming request path: " ++ BS.unpack path
    putStrLn $ "[WS] Incoming headers (subset): " ++ show (take 3 hdrs)
    case parseParams (WS.requestPath req) of
        Nothing -> do
            putStrLn "[WS] Failed to parse roomId/playerId from query"
            conn <- WS.acceptRequest pending
            WS.sendTextData conn ("Error: Missing roomId/playerId" :: T.Text)
            WS.sendClose conn ("Invalid params" :: T.Text)
        Just (roomId, playerId) -> do
            putStrLn $ "[WS] Parsed roomId=" ++ T.unpack roomId ++ ", playerId=" ++ T.unpack playerId
            conn <- WS.acceptRequest pending
            putStrLn $ "New connection: " ++ T.unpack roomId ++ " / " ++ T.unpack playerId
            -- Do not fork here; keep this thread blocked to keep the socket alive
            WS.withPingThread conn 30 (return ()) $
                Handler.handleConnection state conn roomId playerId

-- ======================================================================
-- | Parse query from raw path
-- Expected: /?roomId=ABC123&playerId=xyz789
parseParams :: BS.ByteString -> Maybe (T.Text, T.Text)
parseParams path =
    let (_, qWithQMark) = BS.break (== '?') path
        qs = BS.drop 1 qWithQMark  -- drop '?'
        params = parseQuery qs     -- [(key, Maybe value)]
        findVal k = case lookup k params of
            Just (Just v) -> Just (TE.decodeUtf8 v)
            _ -> Nothing
    in case (findVal "roomId", findVal "playerId") of
        (Just rid, Just pid) -> Just (rid, pid)
        _ -> Nothing
