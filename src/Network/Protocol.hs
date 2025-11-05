{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Message types for real-time communication in 1vs1 mode:
  * Client → Server: Ready, Attack
  * Server → Client: AttackResult, GameOver, Error
-}

module Network.Protocol
    ( -- * Message types
      ClientMessage(..)
    , ServerMessage(..)
    
    -- * Specific message types
    , ReadyMessage(..)
    , AttackMessage(..)
        , StartMessage(..)
    , AttackResultMessage(..)
    , GameOverMessage(..)
    , PlayerReadyMessage(..)
        , GameStartMessage(..)
    , ErrorMessage(..)
    
    -- * Encoding/Decoding
    , decodeClientMessage
    , encodeServerMessage
    ) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import GHC.Generics (Generic)

import Game.Types (Fleet, Position)

-- ============================================================================
-- Client Messages (Client → Server)
-- ============================================================================

-- | Messages sent from client to server
data ClientMessage
    = ReadyMsg ReadyMessage
    | AttackMsg AttackMessage
    | StartMsg StartMessage
    deriving (Show, Eq, Generic)

instance FromJSON ClientMessage
instance ToJSON ClientMessage

-- | Player ready with fleet placement
data ReadyMessage = ReadyMessage
    { rmPlayerId :: Text
    , rmFleet :: Fleet
    } deriving (Show, Eq, Generic)

instance FromJSON ReadyMessage
instance ToJSON ReadyMessage

-- Example JSON:
-- {
--   "type": "ready",
--   "playerId": "abc123",
--   "fleet": [
--     {"shipType": "Destroyer", "shipPosition": {"posRow": 0, "posCol": 0}, ...},
--     ...
--   ]
-- }

-- | Player attack
data AttackMessage = AttackMessage
    { amPlayerId :: Text
    , amPosition :: Position
    } deriving (Show, Eq, Generic)
-- | Host signals to start (navigate to setup)
data StartMessage = StartMessage
    { smPlayerId :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON StartMessage
instance ToJSON StartMessage


instance FromJSON AttackMessage
instance ToJSON AttackMessage

-- Example JSON:
-- {
--   "type": "attack",
--   "playerId": "abc123",
--   "position": {"posRow": 5, "posCol": 3}
-- }

-- ============================================================================
-- Server Messages (Server → Client)
-- ============================================================================

-- | Messages sent from server to client
data ServerMessage
    = AttackResultMsg AttackResultMessage
    | GameOverMsg GameOverMessage
    | GameStartMsg GameStartMessage
    | PlayerReadyMsg PlayerReadyMessage
    | ErrorMsg ErrorMessage
    deriving (Show, Eq, Generic)
-- | Notify both clients to navigate to setup (pre-game placement)
data GameStartMessage = GameStartMessage
    { gsmRoomId :: Text
    } deriving (Show, Eq, Generic)

instance FromJSON GameStartMessage
instance ToJSON GameStartMessage


instance FromJSON ServerMessage
instance ToJSON ServerMessage

-- | Attack result (broadcast to both players)
data AttackResultMessage = AttackResultMessage
    { armAttacker :: Text        -- Attacker playerId
    , armPosition :: Position    -- Attack position
    , armResult :: Text          -- "miss" | "hit" | "sunk"
    , armShipType :: Maybe Text  -- Ship type if sunk
    , armNextTurn :: Text        -- Next player's turn
    } deriving (Show, Eq, Generic)

instance FromJSON AttackResultMessage
instance ToJSON AttackResultMessage

-- Example JSON:
-- {
--   "type": "attack_result",
--   "attacker": "abc123",
--   "position": {"posRow": 5, "posCol": 3},
--   "result": "hit",
--   "shipType": null,
--   "nextTurn": "abc123"
-- }

-- | Game over (broadcast to both players)
data GameOverMessage = GameOverMessage
    { gomWinner :: Text          -- Winner playerId
    , gomWinnerName :: Text      -- Winner name
    , gomReason :: Text          -- "all_ships_sunk" | "opponent_disconnected"
    } deriving (Show, Eq, Generic)

instance FromJSON GameOverMessage
instance ToJSON GameOverMessage
-- | Player ready status update (broadcast to both players)
data PlayerReadyMessage = PlayerReadyMessage
    { prmPlayerId :: Text
    , prmReady :: Bool
    } deriving (Show, Eq, Generic)

instance FromJSON PlayerReadyMessage
instance ToJSON PlayerReadyMessage


-- Example JSON:
-- {
--   "type": "game_over",
--   "winner": "abc123",
--   "winnerName": "Player1",
--   "reason": "all_ships_sunk"
-- }

-- | Error message
data ErrorMessage = ErrorMessage
    { emError :: Text
    , emDetails :: Maybe Text
    } deriving (Show, Eq, Generic)

instance FromJSON ErrorMessage
instance ToJSON ErrorMessage

-- Example JSON:
-- {
--   "type": "error",
--   "error": "Invalid fleet placement",
--   "details": "Ships overlap"
-- }

-- ============================================================================
-- Encoding/Decoding Helpers
-- ============================================================================

-- | Decode client message from JSON ByteString
decodeClientMessage :: BL.ByteString -> Maybe ClientMessage
decodeClientMessage = decode

-- | Encode server message to JSON ByteString
encodeServerMessage :: ServerMessage -> BL.ByteString
encodeServerMessage = encode
