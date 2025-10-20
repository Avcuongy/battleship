{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module State.GameState
    ( -- * Player State
      Player(..)
    , initPlayer
    , setPlayerReady
    , updatePlayerBoard
    
    -- * Turn Management
    , Turn(..)
    , getCurrentPlayer
    , getOpponentPlayer
    , getTurnTimer
    
    -- * Game State
    , GameState(..)
    , GamePhase(..)
    , initGameState
    , updateGameState
    , switchTurn
    , isPlayerTurn
    
    -- * Queries
    , getPlayerById
    , bothPlayersReady
    , getGamePhase
    ) where

import Game.Types
import Game.Board
import Game.Timer
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Aeson
import GHC.Generics
import System.Random (randomRIO) 

-- ===== PLAYER STATE =====

-- | Player in a 1vs1 game
data Player = Player
    { playerId    :: !Text          -- ^ Unique player ID
    , playerName  :: !Text          -- ^ Display name
    , ships       :: ![Ship]        -- ^ Player's ships
    , board       :: !Board         -- ^ Own board (receives attacks)
    , attackBoard :: !Board         -- ^ Tracking attacks on opponent
    , isReady     :: !Bool          -- ^ Ready to start game
    , totalMoves  :: !Int           -- ^ Total moves made
    , shipsLeft   :: ![ShipType]    -- ^ Ships not yet sunk
    } deriving (Show, Generic)

instance ToJSON Player where
    toJSON (Player pid name _ _ _ ready moves shipsLeft) = object
        [ "playerId"   .= pid
        , "playerName" .= name
        , "isReady"    .= ready
        , "totalMoves" .= moves
        , "shipsLeft"  .= shipsLeft
        ]

-- | Initialize player with ID and name
initPlayer :: Text -> Text -> Player
initPlayer pid name = Player
    { playerId    = pid
    , playerName  = name
    , ships       = []
    , board       = emptyBoard
    , attackBoard = emptyBoard
    , isReady     = False
    , totalMoves  = 0
    , shipsLeft   = allShipTypes
    }

-- | Set player ready status
setPlayerReady :: Player -> Bool -> Player
setPlayerReady player ready = player { isReady = ready }

-- | Update player board after attack
updatePlayerBoard :: Player -> Board -> [ShipType] -> Player
updatePlayerBoard player newBoard newShipsLeft = player
    { board = newBoard
    , shipsLeft = newShipsLeft
    }

-- ===== TURN MANAGEMENT =====

-- | Turn state with timer
data Turn 
    = Player1Turn !TimerState  -- ^ Player 1's turn
    | Player2Turn !TimerState  -- ^ Player 2's turn
    deriving (Show, Generic)

instance ToJSON Turn where
    toJSON (Player1Turn timer) = object
        [ "currentPlayer" .= (1 :: Int)
        , "timer" .= timer
        ]
    toJSON (Player2Turn timer) = object
        [ "currentPlayer" .= (2 :: Int)
        , "timer" .= timer
        ]

-- | Get current player number from turn
getCurrentPlayer :: Turn -> Int
getCurrentPlayer (Player1Turn _) = 1
getCurrentPlayer (Player2Turn _) = 2

-- | Get opponent player number
getOpponentPlayer :: Turn -> Int
getOpponentPlayer (Player1Turn _) = 2
getOpponentPlayer (Player2Turn _) = 1

-- | Get timer state from turn
getTurnTimer :: Turn -> TimerState
getTurnTimer (Player1Turn timer) = timer
getTurnTimer (Player2Turn timer) = timer

-- ===== GAME PHASE =====

-- | Game phase
data GamePhase
    = WaitingForPlayers  -- ^ Waiting for second player
    | SetupPhase         -- ^ Players placing ships
    | InProgress         -- ^ Game in progress
    | GameOver !Text     -- ^ Game finished (winner player ID)
    deriving (Show, Eq, Generic)

instance ToJSON GamePhase where
    toJSON WaitingForPlayers = String "waiting_for_players"
    toJSON SetupPhase        = String "setup_phase"
    toJSON InProgress        = String "in_progress"
    toJSON (GameOver winner) = object
        [ "phase"  .= ("game_over" :: Text)
        , "winner" .= winner
        ]

instance FromJSON GamePhase where
    parseJSON = withText "GamePhase" $ \case
        "waiting_for_players" -> pure WaitingForPlayers
        "setup_phase"         -> pure SetupPhase
        "in_progress"         -> pure InProgress
        _                     -> fail "Invalid game phase"

-- ===== COMPLETE GAME STATE =====

-- | Complete game state for 1vs1 match
data GameState = GameState
    { roomId          :: !Text          -- ^ Unique room ID
    , player1         :: !Player        -- ^ Player 1
    , player2         :: !Player        -- ^ Player 2
    , currentTurn     :: !Turn          -- ^ Current turn with timer
    , gamePhase       :: !GamePhase     -- ^ Current game phase
    , consecutiveHits :: !Int           -- ^ Consecutive hits (for same turn bonus)
    , lastMoveTime    :: !UTCTime       -- ^ Last move timestamp
    , turnDuration    :: !Int           -- ^ Seconds per turn
    , moveHistory     :: ![(Text, Position, MoveResult)]  -- ^ (playerId, pos, result)
    } deriving (Show, Generic)

instance ToJSON GameState where
    toJSON (GameState rid p1 p2 turn phase hits lastTime duration _) = object
        [ "roomId"          .= rid
        , "player1"         .= p1
        , "player2"         .= p2
        , "currentTurn"     .= turn
        , "gamePhase"       .= phase
        , "consecutiveHits" .= hits
        , "turnDuration"    .= duration
        ]

-- | Initialize game state with two players
initGameState :: Text -> Text -> Text -> Text -> Text -> Int -> IO GameState
initGameState rid p1Id p1Name p2Id p2Name duration = do
    timer <- startTimer duration
    now <- getCurrentTime
    return $ GameState
        { roomId = rid
        , player1 = initPlayer p1Id p1Name
        , player2 = initPlayer p2Id p2Name
        , currentTurn = Player1Turn timer
        , gamePhase = WaitingForPlayers
        , consecutiveHits = 0
        , lastMoveTime = now
        , turnDuration = duration
        , moveHistory = []
        }

-- | Update game state after a move
updateGameState :: GameState -> Text -> Position -> MoveResult -> GameState
updateGameState state pid pos result =
    let newHistory = (pid, pos, result) : moveHistory state
        newHits = case moveResult result of
            Hit  -> consecutiveHits state + 1
            Sunk -> consecutiveHits state + 1
            _    -> 0
    in state
        { moveHistory = newHistory
        , consecutiveHits = newHits
        }

-- | Switch turn to other player
switchTurn :: GameState -> IO GameState
switchTurn state = do
    newTimer <- startTimer (turnDuration state)
    now <- getCurrentTime
    let newTurn = case currentTurn state of
            Player1Turn _ -> Player2Turn newTimer
            Player2Turn _ -> Player1Turn newTimer
    return $ state
        { currentTurn = newTurn
        , consecutiveHits = 0
        , lastMoveTime = now
        }

-- | Check if it's a specific player's turn
isPlayerTurn :: GameState -> Text -> Bool
isPlayerTurn state pid
    | pid == playerId (player1 state) = getCurrentPlayer (currentTurn state) == 1
    | pid == playerId (player2 state) = getCurrentPlayer (currentTurn state) == 2
    | otherwise = False

-- ===== QUERIES =====

-- | Get player by ID
getPlayerById :: GameState -> Text -> Maybe Player
getPlayerById state pid
    | pid == playerId (player1 state) = Just (player1 state)
    | pid == playerId (player2 state) = Just (player2 state)
    | otherwise = Nothing

-- | Check if both players are ready
bothPlayersReady :: GameState -> Bool
bothPlayersReady state =
    isReady (player1 state) && isReady (player2 state)

-- | Get current game phase
getGamePhase :: GameState -> GamePhase
getGamePhase = gamePhase
