{-# LANGUAGE OverloadedStrings #-}

module API.Routes
    ( setupRoutes
    ) where

import Web.Scotty (ScottyM, post, get, jsonData, param, middleware)
import Network.Wai.Middleware.Static (staticPolicy, addBase, noDots, (>->))
import qualified API.Handlers as Handlers
import qualified State.Manager.Room as RoomMgr
import qualified State.Manager.AI as AIMgr

-- Setup all Scotty routes
setupRoutes :: RoomMgr.RoomManager -> AIMgr.AIManager -> ScottyM ()
setupRoutes roomMgr aiMgr = do
    -- Serve static files from client/ directory
    middleware $ staticPolicy (noDots >-> addBase "client")
    
    -- ========================================================================
    -- Room Routes (1vs1)
    -- ========================================================================
    
    -- POST /api/rooms/create
    -- Body: {"crPlayerId": "abc123", "crPlayerName": "Player1"}
    -- Response: {"crrRoomId": "XyZ789", "crrStatus": "success"}
    post "/api/rooms/create" $ do
        req <- jsonData
        Handlers.createRoomHandler roomMgr req
    
    -- POST /api/rooms/join
    -- Body: {"jrRoomId": "XyZ789", "jrPlayerId": "def456", "jrPlayerName": "Player2"}
    -- Response: {"jrrStatus": "success", "jrrMessage": null}
    post "/api/rooms/join" $ do
        req <- jsonData
        Handlers.joinRoomHandler roomMgr req
    
    -- GET /api/rooms/:id
    -- Response: {"grrRoomId": "XyZ789", "grrGameMode": "1vs1", "grrStatus": "ready", ...}
    get "/api/rooms/:id" $ do
        roomId <- param "id"
        Handlers.getRoomHandler roomMgr roomId
    
    -- ========================================================================
    -- AI Routes
    -- ========================================================================
    
    -- POST /api/ai/start
    -- Body: {"aiPlayerId": "abc123", "aiPlayerName": "Player1", "aiFleet": [...]}
    -- Response: {"asrGameId": "game789", "asrStatus": "success", "asrMessage": null}
    post "/api/ai/start" $ do
        req <- jsonData
        Handlers.startAIHandler aiMgr req
    
    -- POST /api/ai/attack
    -- Body: {"aaGameId": "game789", "aaPosition": {"row": 5, "col": 3}}
    -- Response: {"aarPlayerResult": {...}, "aarAiResult": {...}, "aarGameOver": false, ...}
    post "/api/ai/attack" $ do
        req <- jsonData
        Handlers.processAIAttackHandler aiMgr req
    
    -- ========================================================================
    -- Player Stats Route
    -- ========================================================================
    
    -- POST /api/players/save
    -- Body: {"spPlayerId": "abc123", "spPlayerName": "Player1", "spGamesPlayed": 15, "spWins": 8, "spLosses": 7}
    -- Response: {"sprStatus": "success"}
    post "/api/players/save" $ do
        req <- jsonData
        Handlers.savePlayerHandler req
