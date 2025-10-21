main :: IO ()
main = do
    -- Initialize STM state
    roomMgr <- RoomMgr.newRoomManager
    playerMgr <- PlayerMgr.newPlayerManager
    aiMgr <- AIMgr.newAIManager
    
    -- Start WebSocket server (thread 1)
    forkIO $ WSServer.startWebSocketServer state
    
    -- Start Scotty HTTP server (main thread)
    scotty 3000 $ Routes.setupRoutes
