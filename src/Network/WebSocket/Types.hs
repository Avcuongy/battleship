{-# LANGUAGE OverloadedStrings #-}

module Network.WebSocket.Types
    ( WebSocketState(..)
    ) where

import qualified Network.WebSockets as WS
import Control.Concurrent.STM (TVar)
import qualified Data.Text as T
import qualified Data.Map as Map
import Data.Map (Map)

import State.Manager.Room (RoomManager)
import State.Manager.Player (PlayerManager)

-- | Global WebSocket state
data WebSocketState = WebSocketState
    { wsRoomManager :: RoomManager
    , wsPlayerManager :: PlayerManager
    , wsConnections :: TVar (Map T.Text WS.Connection)
    }
