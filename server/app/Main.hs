{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Data.Aeson
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (newTVarIO)

-- Import local modules
import qualified API as A
import qualified Room.Manager as RM

main :: IO ()
main = do
  putStrLn "BattleShip Server Starting..."
  putStrLn "========================================="
  putStrLn "Server running on http://localhost:3000"
  putStrLn "Serving client from: ../client"
  putStrLn "Open browser: http://localhost:3000\n"
  
  -- Initialize room manager state
  roomMap <- RM.newRoomMap
  
  scotty 3000 $ do
    -- CORS middleware for frontend
    middleware $ cors (const $ Just simpleCorsResourcePolicy 
      { corsRequestHeaders = ["Content-Type"]
      , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
      })
    
    -- Serve static files from client directory
    -- Use absolute path to avoid working directory issues
    middleware $ staticPolicy (addBase "d:/_Dev/_Project/BattleShip/client")
    
    -- ============================================================
    -- ROUTES FOR CLIENT PAGES
    -- ============================================================
    
    -- Root redirects to initial page
    get "/" $ do
      redirect "/pages/initial.html"
    
    -- ============================================================
    -- API ENDPOINTS
    -- ============================================================
    
    -- Player login (Initial page)
    post "/api/login" $ do
      req <- jsonData :: ActionM A.LoginRequest
      result <- liftIO $ A.handleLogin req
      json result
    
    -- Create room (Home page - AI mode)
    post "/api/room/create" $ do
      req <- jsonData :: ActionM A.CreateRoomRequest
      result <- liftIO $ A.handleCreateRoom roomMap req
      json result
    
    -- Set player ready (Loading page)
    post "/api/room/ready" $ do
      req <- jsonData :: ActionM A.SetReadyRequest
      result <- liftIO $ A.handleSetReady roomMap req
      json result
    
    -- Complete game (Game page - when game ends)
    post "/api/game/complete" $ do
      req <- jsonData :: ActionM A.GameResult
      result <- liftIO $ A.handleGameComplete roomMap req
      json result
