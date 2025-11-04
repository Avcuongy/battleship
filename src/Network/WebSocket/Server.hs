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

import qualified Network.WebSocket.Handler as Handler
import Network.WebSocket.Types (WebSocketState(..))

-- ======================================================================
-- | Start WebSocket server
startWebSocketServer :: WebSocketState -> IO ()
startWebSocketServer state = do
    putStrLn "WebSocket server running on port 9160"
    run 9160 $ websocketsOr
        WS.defaultConnectionOptions
        (wsApp state)
        backupApp

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
