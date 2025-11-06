{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Types
    ( -- Request types
      CreateRoomRequest(..)
    , JoinRoomRequest(..)
    , AIStartRequest(..)
    , AIAttackRequest(..)
    , SavePlayerRequest(..)
    
    -- Response types
    , CreateRoomResponse(..)
    , JoinRoomResponse(..)
    , GetRoomResponse(..)
    , AIStartResponse(..)
    , AIAttackResponse(..)
    , SavePlayerResponse(..)
    , GeneratePlayerIdResponse(..)
    , ErrorResponse(..)
    
    -- Common types
    , AttackResult(..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Game.Types (Fleet, Position)

-- ============================================================================
-- Request Types
-- ============================================================================

-- POST /api/rooms/create
data CreateRoomRequest = CreateRoomRequest
    { crPlayerId :: Text    -- Player 1 ID (6 chars a-zA-Z)
    , crPlayerName :: Text  -- Player 1 name
    } deriving (Show, Generic)

instance FromJSON CreateRoomRequest
instance ToJSON CreateRoomRequest

-- POST /api/rooms/join
data JoinRoomRequest = JoinRoomRequest
    { jrRoomId :: Text      -- Room ID to join (6 chars case-sensitive)
    , jrPlayerId :: Text    -- Player 2 ID
    , jrPlayerName :: Text  -- Player 2 name
    } deriving (Show, Generic)

instance FromJSON JoinRoomRequest
instance ToJSON JoinRoomRequest

-- POST /api/ai/start
data AIStartRequest = AIStartRequest
    { aiPlayerId :: Text    -- Player ID
    , aiPlayerName :: Text  -- Player name
    , aiFleet :: Fleet      -- Player's 5 ships
    } deriving (Show, Generic)

instance FromJSON AIStartRequest
instance ToJSON AIStartRequest

-- POST /api/ai/attack
data AIAttackRequest = AIAttackRequest
    { aaGameId :: Text      -- AI game session ID
    , aaPosition :: Position -- Attack position (row, col)
    } deriving (Show, Generic)

instance FromJSON AIAttackRequest
instance ToJSON AIAttackRequest

-- POST /api/players/save
data SavePlayerRequest = SavePlayerRequest
    { spPlayerId :: Text
    , spPlayerName :: Text
    , spGamesPlayed :: Int
    , spWins :: Int
    , spLosses :: Int
    } deriving (Show, Generic)

instance FromJSON SavePlayerRequest
instance ToJSON SavePlayerRequest

-- ============================================================================
-- Response Types
-- ============================================================================

-- Response: POST /api/rooms/create
data CreateRoomResponse = CreateRoomResponse
    { crrRoomId :: Text     -- Generated room ID
    , crrStatus :: Text     -- "success"
    } deriving (Show, Generic)

instance FromJSON CreateRoomResponse
instance ToJSON CreateRoomResponse

-- Response: POST /api/rooms/join
data JoinRoomResponse = JoinRoomResponse
    { jrrStatus :: Text         -- "success" | "error"
    , jrrMessage :: Maybe Text  -- Error message if failed
    } deriving (Show, Generic)

instance FromJSON JoinRoomResponse
instance ToJSON JoinRoomResponse

-- Response: GET /api/rooms/:id
data GetRoomResponse = GetRoomResponse
    { grrRoomId :: Text
    , grrGameMode :: Text       -- "1vs1"
    , grrStatus :: Text         -- "ready" | "in_progress" | "done"
    , grrPlayer1Id :: Text
    , grrPlayer1Name :: Text
    , grrPlayer1Ready :: Bool
    , grrPlayer2Id :: Maybe Text
    , grrPlayer2Name :: Maybe Text
    , grrPlayer2Ready :: Maybe Bool
    , grrCurrentTurn :: Maybe Text  -- current player's ID if game in progress
    } deriving (Show, Generic)

instance FromJSON GetRoomResponse
instance ToJSON GetRoomResponse

-- Response: POST /api/ai/start
data AIStartResponse = AIStartResponse
    { asrGameId :: Text         -- AI session ID
    , asrStatus :: Text         -- "success" | "error"
    , asrMessage :: Maybe Text  -- Error message if validation failed
    } deriving (Show, Generic)

instance FromJSON AIStartResponse
instance ToJSON AIStartResponse

-- Response: POST /api/ai/attack
data AIAttackResponse = AIAttackResponse
    { aarPlayerResult :: AttackResult   -- Player's attack result
    , aarAiResult :: AttackResult       -- AI's attack result
    , aarGameOver :: Bool               -- Game ended?
    , aarWinner :: Maybe Text           -- "player" | "ai" | Nothing
    } deriving (Show, Generic)

instance FromJSON AIAttackResponse
instance ToJSON AIAttackResponse

-- Response: POST /api/players/save
data SavePlayerResponse = SavePlayerResponse
    { sprStatus :: Text  -- "success"
    } deriving (Show, Generic)

instance FromJSON SavePlayerResponse
instance ToJSON SavePlayerResponse

-- Response: GET /api/players/generate-id
data GeneratePlayerIdResponse = GeneratePlayerIdResponse
    { gpiPlayerId :: Text  -- Generated unique player ID (6 chars)
    } deriving (Show, Generic)

instance FromJSON GeneratePlayerIdResponse
instance ToJSON GeneratePlayerIdResponse

-- Generic error response
data ErrorResponse = ErrorResponse
    { errStatus :: Text   -- "error"
    , errMessage :: Text  -- Error description
    } deriving (Show, Generic)

instance FromJSON ErrorResponse
instance ToJSON ErrorResponse

-- ============================================================================
-- Common Types
-- ============================================================================

-- Attack result (used in responses)
data AttackResult = AttackResult
    { arPosition :: Position  -- Attacked position
    , arResult :: Text        -- "miss" | "hit" | "sunk"
    , arShipType :: Maybe Text -- Ship type if sunk (optional)
    } deriving (Show, Generic)

instance FromJSON AttackResult
instance ToJSON AttackResult
