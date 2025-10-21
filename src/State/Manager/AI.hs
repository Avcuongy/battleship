{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module State.AIState
    ( -- * AI Game State
      AIGameState(..)
    , AIGamePhase(..)
    , PlayerTurn(..)
    
    -- * Initialization
    , initAIGameState
    
    -- * Updates
    , updateAIGameState
    , setAIGamePhase
    , switchAITurn
    
    -- * Storage
    , aiGamesRef
    , getAIGame
    , addAIGame
    , removeAIGame
    , updateAIGame
    ) where

import Game.Types
import Game.Board
import Game.AI (AIState, initAIState)
import qualified Game.AI as AI
import Data.Text (Text)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Aeson
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)

-- ===== TYPES =====

-- | AI game phase
data AIGamePhase
    = AISetup       -- ^ Player setting up ships
    | AIInProgress  -- ^ Game in progress
    | AIGameOver !Text  -- ^ Game finished (winner: "player" or "ai")
    deriving (Show, Eq, Generic)

instance ToJSON AIGamePhase where
    toJSON AISetup = String "setup"
    toJSON AIInProgress = String "in_progress"
    toJSON (AIGameOver winner) = object
        [ "phase"  .= ("game_over" :: Text)
        , "winner" .= winner
        ]

-- | Turn in AI game
data PlayerTurn
    = HumanTurn  -- ^ Player's turn
    | AITurn     -- ^ AI's turn
    deriving (Show, Eq, Generic)

instance ToJSON PlayerTurn where
    toJSON HumanTurn = String "human"
    toJSON AITurn    = String "ai"

-- | Complete AI game state
data AIGameState = AIGameState
    { gameId          :: !Text          -- ^ Unique game ID
    , playerId        :: !Text          -- ^ Player ID
    , playerName      :: !Text          -- ^ Player name
    , playerShips     :: ![Ship]        -- ^ Player's ships
    , playerBoard     :: !Board         -- ^ Player's board (receives AI attacks)
    , playerAttackBoard :: !Board       -- ^ Track player attacks on AI
    , aiShips         :: ![Ship]        -- ^ AI's ships
    , aiBoard         :: !Board         -- ^ AI's board (receives player attacks)
    , aiState         :: !AIState       -- ^ AI decision state
    , currentTurn     :: !PlayerTurn    -- ^ Whose turn
    , gamePhase       :: !AIGamePhase   -- ^ Current phase
    , playerShipsLeft :: ![ShipType]    -- ^ Player ships not sunk
    , aiShipsLeft     :: ![ShipType]    -- ^ AI ships not sunk
    , moveHistory     :: ![(Text, Position, MoveResult)]  -- ^ Move history
    } deriving (Show, Generic)

instance ToJSON AIGameState where
    toJSON (AIGameState gid pid pname _ _ _ _ _ _ turn phase pShipsLeft aiShipsLeft _) = object
        [ "gameId"          .= gid
        , "playerId"        .= pid
        , "playerName"      .= pname
        , "currentTurn"     .= turn
        , "gamePhase"       .= phase
        , "playerShipsLeft" .= pShipsLeft
        , "aiShipsLeft"     .= aiShipsLeft
        ]

-- ===== INITIALIZATION =====

-- | Initialize AI game state
initAIGameState :: Text -> Text -> Text -> [Ship] -> [Ship] -> AIGameState
initAIGameState gid pid pname playerShips aiShips = AIGameState
    { gameId = gid
    , playerId = pid
    , playerName = pname
    , playerShips = playerShips
    , playerBoard = initBoardWithShips playerShips
    , playerAttackBoard = emptyBoard
    , aiShips = aiShips
    , aiBoard = initBoardWithShips aiShips
    , aiState = initAIState
    , currentTurn = HumanTurn
    , gamePhase = AISetup
    , playerShipsLeft = allShipTypes
    , aiShipsLeft = allShipTypes
    , moveHistory = []
    }

-- ===== UPDATES =====

-- | Update AI game state after a move
updateAIGameState :: AIGameState -> Text -> Position -> MoveResult -> AIGameState
updateAIGameState state actor pos result =
    let newHistory = (actor, pos, result) : moveHistory state
    in state { moveHistory = newHistory }

-- | Set game phase
setAIGamePhase :: AIGameState -> AIGamePhase -> AIGameState
setAIGamePhase state phase = state { gamePhase = phase }

-- | Switch turn
switchAITurn :: AIGameState -> AIGameState
switchAITurn state =
    let newTurn = case currentTurn state of
            HumanTurn -> AITurn
            AITurn    -> HumanTurn
    in state { currentTurn = newTurn }

-- ===== GLOBAL STORAGE (IORef) =====

-- | Global storage for AI games (no STM needed - single player)
{-# NOINLINE aiGamesRef #-}
aiGamesRef :: IORef (M.Map Text AIGameState)
aiGamesRef = unsafePerformIO $ newIORef M.empty

-- | Get AI game by ID
getAIGame :: Text -> IO (Maybe AIGameState)
getAIGame gid = do
    games <- readIORef aiGamesRef
    return $ M.lookup gid games

-- | Add new AI game
addAIGame :: Text -> AIGameState -> IO ()
addAIGame gid state = do
    atomicModifyIORef' aiGamesRef $ \games ->
        (M.insert gid state games, ())

-- | Remove AI game
removeAIGame :: Text -> IO ()
removeAIGame gid = do
    atomicModifyIORef' aiGamesRef $ \games ->
        (M.delete gid games, ())

-- | Update AI game state
updateAIGame :: Text -> (AIGameState -> AIGameState) -> IO ()
updateAIGame gid updateFn = do
    atomicModifyIORef' aiGamesRef $ \games ->
        (M.adjust updateFn gid games, ())
