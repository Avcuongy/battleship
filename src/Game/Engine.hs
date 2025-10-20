{-# LANGUAGE OverloadedStrings #-}

module Game.Engine
    ( -- * Types
      PlayerState(..)
    , GameState(..)
    
    -- * Initialization
    , initPlayerState
    , initGameState
    
    -- * Game Operations
    , processMove
    , processAttack
    , isGameOver
    , getWinner
    , switchTurn
    , updateAfterMove
    
    -- * Queries
    , canMakeMove
    , getPlayerMoves
    , getRemainingShips
    , getGameProgress
    ) where

import Game.Types
import Game.Board
import Game.Ship
import qualified Data.Map.Strict as M

-- ===== TYPES =====

-- | State for a single player
data PlayerState = PlayerState
    { playerShips        :: ![Ship]        -- ^ Player's ships
    , playerBoard        :: !Board         -- ^ Player's own board (receives attacks)
    , opponentTrackBoard :: !Board         -- ^ Track attacks on opponent
    , totalMoves         :: !Int           -- ^ Number of moves made
    , shipsRemaining     :: ![ShipType]    -- ^ Ships not yet sunk
    } deriving (Show)

-- | Complete game state (for 1v1 or vs AI)
data GameState = GameState
    { player1State :: !PlayerState
    , player2State :: !PlayerState
    , currentTurn  :: !Int           -- ^ 1 = player1, 2 = player2/AI
    , gameStatus   :: !GameStatus
    , moveHistory  :: ![(Int, Position, MoveResult)]  -- ^ (player, pos, result)
    } deriving (Show)

-- ===== INITIALIZATION =====

-- | Initialize player state with ship placement
initPlayerState :: [Ship] -> Either String PlayerState
initPlayerState ships = do
    validatedShips <- validateSetup ships
    let board = initBoardWithShips validatedShips
    return $ PlayerState
        { playerShips = validatedShips
        , playerBoard = board
        , opponentTrackBoard = emptyBoard
        , totalMoves = 0
        , shipsRemaining = allShipTypes
        }

-- | Initialize game state with both players
initGameState :: PlayerState -> PlayerState -> GameState
initGameState p1 p2 = GameState
    { player1State = p1
    , player2State = p2
    , currentTurn = 1
    , gameStatus = Ongoing
    , moveHistory = []
    }

-- ===== GAME OPERATIONS =====

-- | Check if a move can be made at position (not already attacked)
canMakeMove :: PlayerState -> Position -> Bool
canMakeMove state pos =
    case getCell (opponentTrackBoard state) pos of
        Just cell -> cellState cell == Empty
        Nothing -> False

-- | Process a move: attacker makes move on defender's board
-- Returns: (new attacker state, new defender state, move result)
processMove :: PlayerState -> PlayerState -> Position 
            -> (PlayerState, PlayerState, MoveResult)
processMove attackerState defenderState pos =
    let (newDefenderState, result) = processAttack defenderState pos
        newAttackerState = updateAttackerTracking attackerState pos result
    in (newAttackerState, newDefenderState, result)

-- | Process attack on defender's board
processAttack :: PlayerState -> Position -> (PlayerState, MoveResult)
processAttack defenderState pos =
    case getCell (playerBoard defenderState) pos of
        Nothing -> 
            -- Should not happen if position is valid
            (defenderState, MoveResult pos Miss Nothing)
        Just cell -> 
            case cellShip cell of
                Nothing -> 
                    -- Miss: no ship at position
                    let newBoard = updateCell (playerBoard defenderState) pos Miss
                    in ( defenderState { playerBoard = newBoard }
                       , MoveResult pos Miss Nothing
                       )
                Just shipType -> 
                    -- Hit: ship at position
                    let newBoard = updateCell (playerBoard defenderState) pos Hit
                        sunk = isShipSunk newBoard shipType
                        finalBoard = if sunk
                                     then markShipAsSunk newBoard shipType
                                     else newBoard
                        resultState = if sunk then Sunk else Hit
                        sunkShipResult = if sunk then Just shipType else Nothing
                        newShipsRemaining = if sunk
                                           then filter (/= shipType) (shipsRemaining defenderState)
                                           else shipsRemaining defenderState
                    in ( defenderState 
                            { playerBoard = finalBoard
                            , shipsRemaining = newShipsRemaining
                            }
                       , MoveResult pos resultState sunkShipResult
                       )

-- | Update attacker's tracking board after move
updateAttackerTracking :: PlayerState -> Position -> MoveResult -> PlayerState
updateAttackerTracking state pos result =
    let newTrackBoard = updateCell (opponentTrackBoard state) pos (moveResult result)
        newMoveCount = totalMoves state + 1
    in state
        { opponentTrackBoard = newTrackBoard
        , totalMoves = newMoveCount
        }

-- | Check if game is over (all ships of a player sunk)
isGameOver :: PlayerState -> Bool
isGameOver state = null (shipsRemaining state)

-- | Determine winner from game state
getWinner :: GameState -> Maybe GameStatus
getWinner state
    | gameStatus state /= Ongoing = Just (gameStatus state)
    | isGameOver (player1State state) = Just Player2Won
    | isGameOver (player2State state) = Just Player1Won
    | otherwise = Nothing

-- | Switch turn to other player
switchTurn :: GameState -> GameState
switchTurn state =
    state { currentTurn = if currentTurn state == 1 then 2 else 1 }

-- | Update game state after a move
updateAfterMove :: GameState -> Int -> Position -> MoveResult -> GameState
updateAfterMove state playerNum pos result =
    let newHistory = (playerNum, pos, result) : moveHistory state
        newStatus = case getWinner state of
                        Just status -> status
                        Nothing -> Ongoing
    in state 
        { moveHistory = newHistory
        , gameStatus = newStatus
        }

-- ===== QUERIES =====

-- | Get total moves made by player
getPlayerMoves :: PlayerState -> Int
getPlayerMoves = totalMoves

-- | Get remaining ships for player
getRemainingShips :: PlayerState -> [ShipType]
getRemainingShips = shipsRemaining

-- | Get game progress (0.0 to 1.0)
-- Based on total hits out of 17 total ship cells
getGameProgress :: PlayerState -> Double
getGameProgress state =
    let totalShipCells = 17  -- 5+4+3+3+2
        hits = getHitCount (playerBoard state)
    in fromIntegral hits / fromIntegral totalShipCells
