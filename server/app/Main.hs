{-# LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Network.Wai.Middleware.Static
import Network.Wai.Middleware.Cors
import Data.Aeson
import Control.Monad.IO.Class (liftIO)

-- Import local modules
import qualified Session as S
import qualified Lib as L
import qualified WebAPI as W

main :: IO ()
main = do
  putStrLn "Starting BattleShip server on port 3000..."
  scotty 3000 $ do
    middleware $ cors (const $ Just simpleCorsResourcePolicy)
    middleware $ staticPolicy (addBase "app")
    
    get "/" $ file "app/templates/initial.html"
    
    post "/api/login" $ do
      req <- jsonData :: ActionM W.LoginRequest
      result <- liftIO $ W.loginHandler req
      json result