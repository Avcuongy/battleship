{-# LANGUAGE DeriveGeneric #-}

{-|
Types for application state managed by STM (Software Transactional Memory):
  * RoomState: Game room information (1vs1)
  * PlayerInfo: Player connection and metadata
  * AIGameState: AI game session data
-}

module State.Types
    ( -- * Room types
      RoomState(..)
    , GameStatus(..)
    
    -- * Player types
    , PlayerInfo(..)
    
    -- * AI types
    , AIGameState(..)
    
    -- * Type aliases
    , RoomId
    , PlayerId
    , GameId
    ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS

import Game.Types (Board, Fleet)

-- ============================================================================
-- Type Aliases
-- ============================================================================

type RoomId = Text    -- 6 chars a-zA-Z (case-sensitive)
type PlayerId = Text  -- 6 chars a-zA-Z
type GameId = Text    -- AI session ID

-- ============================================================================
-- Game Status
-- ============================================================================

-- | Room/Game status
data GameStatus
    = Ready       -- Waiting for players to place ships
    | InProgress  -- Game is active
    | Done        -- Game finished
    deriving (Show, Eq, Generic)

-- ============================================================================
-- Room State (1vs1)
-- ============================================================================

-- | Room state for 1vs1 games
data RoomState = RoomState
    { roomId :: !RoomId
    , gameMode :: !Text                  -- Always "1vs1"
    , status :: !GameStatus
    
    -- Player 1
    , player1Id :: !PlayerId
    , player1Name :: !Text
    , player1Ready :: !Bool
    , player1Fleet :: !(Maybe Fleet)
    , player1Board :: !(Maybe Board)
    
    -- Player 2
    , player2Id :: !(Maybe PlayerId)
    , player2Name :: !(Maybe Text)
    , player2Ready :: !Bool
    , player2Fleet :: !(Maybe Fleet)
    , player2Board :: !(Maybe Board)
    
    -- Game state
    , currentTurn :: !(Maybe PlayerId)   -- Whose turn is it?
    , turnStartTime :: !(Maybe UTCTime)  -- When did current turn start?
    , createdAt :: !UTCTime
    } deriving (Show, Generic)

-- ============================================================================
-- Player Info
-- ============================================================================

-- | Player information (connection + metadata)
data PlayerInfo = PlayerInfo
    { playerId :: !PlayerId
    , playerName :: !Text
    , currentRoomId :: !(Maybe RoomId)
    , playerConnection :: !(Maybe WS.Connection)
    } deriving (Generic)

-- Note: Can't derive Show for WS.Connection
instance Show PlayerInfo where
    show pi = "PlayerInfo {playerId=" ++ show (playerId pi)
           ++ ", playerName=" ++ show (playerName pi)
           ++ ", currentRoomId=" ++ show (currentRoomId pi)
           ++ ", connected=" ++ show (maybe False (const True) (playerConnection pi))
           ++ "}"

-- ============================================================================
-- AI Game State
-- ============================================================================

-- | AI game session state
data AIGameState = AIGameState
    { aiGameId :: !GameId
    , aiPlayerId :: !PlayerId
    , aiPlayerName :: !Text
    , aiPlayerBoard :: !Board      -- Player's board
    , aiOpponentBoard :: !Board    -- AI's board (fixed fleet)
    , aiCurrentTurn :: !Text       -- "player" | "ai"
    , aiCreatedAt :: !UTCTime
    } deriving (Show, Generic)
