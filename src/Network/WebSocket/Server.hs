{-# LANGUAGE OverloadedStrings #-}

module Network.WebSocket.Server
    ( startWebSocketServer
    , WebSocketState(..)
    ) where

import qualified Network.WebSockets as WS
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai (Application, responseLBS)
import Network.HTTP.Types (status400)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)
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
        query = WS.requestHeaders req
    case parseParams (WS.requestPath req) of
        Nothing -> do
            conn <- WS.acceptRequest pending
            WS.sendTextData conn ("Error: Missing roomId/playerId" :: T.Text)
            WS.sendClose conn ("Invalid params" :: T.Text)
        Just (roomId, playerId) -> do
            conn <- WS.acceptRequest pending
            putStrLn $ "➡️  New connection: " ++ T.unpack roomId ++ " / " ++ T.unpack playerId
            _ <- forkIO $ 
                WS.withPingThread conn 30 (return ()) $
                    Handler.handleConnection state conn roomId playerId
            return ()

-- ======================================================================
-- | Parse query from raw path
-- Expected: /?roomId=ABC123&playerId=xyz789
parseParams :: BS.ByteString -> Maybe (T.Text, T.Text)
parseParams path =
    let queryStr = BS.dropWhile (/= '?') path
        params = BS.split '&' (BS.drop 1 queryStr)
        parsedParams = [BS.break (== '=') p | p <- params]
        getVal key = fmap (TE.decodeUtf8 . BS.drop 1) (lookup key parsedParams)
    in case (getVal "roomId", getVal "playerId") of
        (Just rid, Just pid) -> Just (rid, pid)
        _ -> Nothing
