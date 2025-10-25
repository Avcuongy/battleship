module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, newTVarIO)
import qualified Data.Map as Map
import Web.Scotty (scotty)

import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.Player as PlayerMgr
import qualified State.Manager.AI as AIMgr
import qualified Network.WebSocket.Server as WSServer
import Network.WebSocket.Types (WebSocketState(..))
import qualified API.Routes as Routes

main :: IO ()
main = do
    -- Initialize STM state
    roomMgr <- RoomMgr.newRoomManager
    playerMgr <- PlayerMgr.newPlayerManager
    aiMgr <- AIMgr.newAIManager
    
    -- Create WebSocket state
    wsConnections <- newTVarIO Map.empty
    let state = WebSocketState
            { wsRoomManager = roomMgr
            , wsPlayerManager = playerMgr
            , wsConnections = wsConnections
            }
    
    -- Start WebSocket server (thread 1)
    _ <- forkIO $ WSServer.startWebSocketServer state
    
    -- Start Scotty HTTP server (main thread)
    putStrLn "BattleShip server starting..."
    putStrLn " HTTP API: http://localhost:3000"
    putStrLn " WebSocket: ws://localhost:9160"
    scotty 3000 $ Routes.setupRoutes roomMgr aiMgr playerMgr
